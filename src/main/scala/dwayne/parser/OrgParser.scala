package dwayne.parser

import cats.catsInstancesForId
import cats.syntax.all.*
import cats.Id
import dwayne.data.Forest
import dwayne.org.ScheduledType
import dwayne.org.Task
import monocle.Lens
import scala.util.chaining.*
import scala.util.Failure
import scala.util.Success

case class OrgTaskTitle(
    level: Int,
    state: Option[String],
    priority: Option[Int],
    title: String,
    tags: List[String]
)

enum PropertyDelimeter(val str: String) {
  case Start extends PropertyDelimeter(":PROPERTIES:")
  case End extends PropertyDelimeter(":END:")
}

enum PairedSeparator(val left: String, val right: String) {
  case Brackets extends PairedSeparator(left = "\\[", right = "\\]")
  case Triangles extends PairedSeparator(left = "<", right = ">")
}

enum ScheduledField(val name: String, val separator: PairedSeparator, val lense: Lens[Task, Option[ScheduledType]]) {
  case Scheduled extends ScheduledField("SCHEDULED", PairedSeparator.Triangles, Task.scheduledLense)
  case Deadline extends ScheduledField("DEADLINE", PairedSeparator.Triangles, Task.deadlineLense)
  case Closed extends ScheduledField("CLOSED", PairedSeparator.Brackets, Task.closedLense)
}

case class StopCondition(msg: String) {
  override def toString = f"Should have stopped: $msg"
}

case class UnexpectedError(msg: String) {
  override def toString = f"UNEXPECTED: $msg"
}

case class InvalidSyntaxError(msg: String, location: Location) {
  override def toString = f"Error: $msg at ${location.line}:${location.column}"
}

type ParseResult[T] = Either[InvalidSyntaxError | UnexpectedError, T]

object OrgParser {

  // Utilities

  def unnulifyAndStrip(s: String): Option[String] =
    Option(s).map(_.strip).filter(_.nonEmpty)
  private val titleLineRegex =
    "(\\*+)\\s+(\\b[A-Z]+\\b)?\\s*(\\[#[A-Z]\\])?\\s*(.*?)\\s*(:(\\w+:)+)?$".r

  // Parsers
  //
  private val skipBlankLines = new Parser[Id, Unit] {
    def parse(in: PInput) = {
      val (skippedLines, restLines) = in.text.lines.span(_.trim.isEmpty)
      val skipLines                 = Location.modifyLine(_ + skippedLines.length)
      PResult((), PInput(Text(restLines), skipLines(in.location)))
    }
  }

  private val skipBlankSpaces = new Parser[Id, Unit] {
    def parse(in: PInput) =
      in.text.lines match {
        case Nil => PResult((), PInput(Text.empty, in.location))
        case l :: rest =>
          val newL        = l.stripLeading
          val skipColumns = Location.modifyColumn(_ + (l.length - newL.length))
          PResult((), PInput(Text(newL :: rest), skipColumns(in.location)))
      }
  }

  private val skipBlanks = skipBlankLines >> skipBlankSpaces

  private val taskTitleParser = skipBlanks.pure[ParseResult] >>
    new Parser[ParseResult, OrgTaskTitle] {
      def parse(in: PInput) =
        in.text.lines match {
          case titleLineRegex(a, st, sp, t, ts, _) :: tail =>
            val level = a.length
            val state = unnulifyAndStrip(st)

            def priorityStringToInt(s: String): Option[Int] =
              if (s.length() == 4) (s.charAt(2) - 'A').some else None
            val priority = unnulifyAndStrip(sp).flatMap(priorityStringToInt)

            val title = unnulifyAndStrip(t).getOrElse("")

            def parseTags(s: String): List[String] =
              s.split(":").toList.filter(_.nonEmpty)
            val tags = unnulifyAndStrip(ts).toList.flatMap(parseTags)

            PResult(OrgTaskTitle(level, state, priority, title, tags), PInput(Text(tail), in.location)).asRight
          case _ => InvalidSyntaxError(f"Unable to parse title string", in.location).asLeft
        }
    }

  // TODO: handle different recurrence formats
  private def timeParser(field: ScheduledField) = skipBlankSpaces.pure[ParseResult] >>
    new Parser[ParseResult, Task => Task] {
      def parse(in: PInput) = {
        val regex =
          f"${field.name}: ${field.separator.left}([^${field.separator.right}]*)${field.separator.right}(.*)".r
        in.text.lines match {
          case regex(time, left) :: rest =>
            ScheduledType.bestMatch(time) match {
              case Success(parsedTime) =>
                PResult(
                  field.lense.replace(parsedTime.some),
                  PInput(Text(left +: rest), in.location)
                ).asRight
              case Failure(e) => InvalidSyntaxError(f"Unable to parse time string: $time", in.location).asLeft
            }
          case _ => InvalidSyntaxError(f"Unable to parse time string", in.location).asLeft
        }
      }
    }

  private def lineConsumer(ss: String) = skipBlankSpaces.pure[ParseResult] >>
    new Parser[ParseResult, Unit] {
      def parse(in: PInput) = {
        val regex = s"^$ss$$".r
        in.text.lines match {
          case str :: rest if regex.matches(str) =>
            PResult((), PInput(Text(rest), Location.nextLine(in.location))).asRight
          case _ => InvalidSyntaxError(f"Expected '$ss', but didn't find it", in.location).asLeft
        }
      }
    }

  private val propertyParser = (
    skipBlankSpaces.pure[[T] =>> Either[StopCondition | InvalidSyntaxError, T]] >>
      new Parser[[T] =>> Either[StopCondition | InvalidSyntaxError, T], (String, String)] {
        def parse(in: PInput) = {
          val regex = "^:([a-zA-Z0-9]+): (.*)$".r
          in.text.lines match {
            case regex(k, v) :: rest               => PResult((k, v), PInput(Text(rest), in.location.nextLine)).asRight
            case PropertyDelimeter.End.str :: rest => StopCondition("End of properties").asLeft
            case s :: rest => InvalidSyntaxError(f"Unable to parse property string '$s'", in.location).asLeft
            case Nil =>
              InvalidSyntaxError(f"Reached end of file, but properties section didn't end", in.location).asLeft
          }
        }
      }
  )
    .repeatedUntil {
      case Left(StopCondition(_)) => true
      case _                      => false
    }
    .morph[ParseResult] {
      case Left(StopCondition(msg))           => UnexpectedError(f"Should not have stopped: $msg").asLeft
      case Left(InvalidSyntaxError(msg, loc)) => InvalidSyntaxError(msg, loc).asLeft
      case Right(v)                           => v.asRight
    }
    .map(_.toMap)

  private val bodyParser = new Parser[[T] =>> Either[StopCondition, T], String] {
    def parse(in: PInput) =
      in.text.lines match {
        case Nil => StopCondition("Consumed everything").asLeft
        case head :: rest if titleLineRegex.matches(head) => StopCondition("Start of the next task").asLeft
        case head :: rest => PResult(head, PInput(Text(rest), Location.nextLine(in.location))).asRight
      }
  }.repeatedUntil {
    case Left(StopCondition(_)) => true
    case _                      => false
  }.morph[ParseResult](_.fold(e => UnexpectedError(f"Should not have stopped: ${e.msg}").asLeft, _.asRight))

  private val dateTimeParser: Parser[ParseResult, Task => Task] = Parser
    .parallel[ParseResult, Task => Task](
      _.isRight,
      ScheduledField.values.map(timeParser).toList
    )
    .repeatedUntil(_.value.fold(_ => true, _.isEmpty))
    .morph(_.value.fold(_.asLeft, _.fold(UnexpectedError("Did not find any scheduling").asLeft)(_.asRight)))
    .map(_.fold(identity[Task])((f, acc) => acc.andThen(f)))

  val taskParser = for {
    orgTaskTitle <- taskTitleParser
    addDateTimes <- dateTimeParser
    _ <- lineConsumer(PropertyDelimeter.Start.str)
    properties <- propertyParser
    _ <- lineConsumer(PropertyDelimeter.End.str)
    bodyLines <- bodyParser
  } yield Task(
    level = orgTaskTitle.level,
    state = orgTaskTitle.state,
    priority = orgTaskTitle.priority,
    title = orgTaskTitle.title,
    tags = orgTaskTitle.tags,
    scheduled = None,
    deadline = None,
    closed = None,
    properties = properties,
    body = bodyLines.mkString("\n")
  ).pipe(addDateTimes)

  val taskForest = taskParser.withLocation
    .repeatedUntil(_.isLeft)
    .flatMap(tasksWithLocation =>
      new Parser[ParseResult, Forest[Task]] {
        def parse(in: PInput): ParseResult[PResult[Forest[Task]]] =
          Forest
            .fromList(tasksWithLocation, _._1.level)
            .fold(
              e => InvalidSyntaxError(f"Unable construct tree: ${e.toString}", e.getValue._2).asLeft,
              f => PResult(f.map(_._1), in).asRight
            )
      }
    )

}
