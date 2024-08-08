package dwayne

import cats.syntax.all.*
import dwayne.codec.asIso
import dwayne.codec.Codec
import dwayne.codec.CodecError
import dwayne.codec.IsoParser
import dwayne.codec.Location
import dwayne.codec.Parser
import dwayne.codec.ParserInput
import dwayne.codec.ParserResult
import dwayne.codec.TransformParser
import dwayne.codec.ValidatedCodec
import dwayne.data.Forest
import dwayne.data.NodeAppendingError
import dwayne.org.Properties
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.Locale
import monocle.Lens
import scala.util.Failure
import scala.util.Success

enum PairedSeparator(val left: String, val right: String) {
  case Brackets extends PairedSeparator(left = "\\[", right = "\\]")
  case Triangles extends PairedSeparator(left = "<", right = ">")
}

package org {
  //  --------------- helper functions -----------------------
  def unnulifyAndStrip(s: String): Option[String] =
    Option(s).map(_.strip).filter(_.nonEmpty)

  def priorityStringToInt(s: String): Option[Int] =
    if (s.length() == 4) (s.charAt(2) - 'A').some else None

  def parseTags(s: String): List[String] =
    s.split(":").toList.filter(_.nonEmpty)

  val titleLineRegex =
    "(\\*+)\\s+(\\b[A-Z]+\\b)?\\s*(\\[#[A-Z]\\])?\\s*(.*?)\\s*(:(\\w+:)+)?$".r

  def skipBlank(s: String): (String, Location => Location) = {
    val lines                     = s.split("\n")
    val (skippedLines, restLines) = lines.span(_.trim.isEmpty)
    val skipLines                 = Location.modifyLine(_ + skippedLines.length)
    restLines.toList match {
      case Nil => (s, skipLines)
      case l :: rest =>
        val newL        = l.stripLeading
        val skipColumns = Location.modifyColumn(_ + (l.length - newL.length))
        ((newL :: rest).mkString("\n"), skipColumns.andThen(skipLines))
    }
  }

  //  ------------------ parsers -------------------------

  private val titleParser = new IsoParser[Task] {
    def parse(s: ParserInput) = {
      val (lines, modifyLocation) = skipBlank(s.str)
      val newLocation             = modifyLocation(s.location)
      lines.split("\n").toList match {
        case titleLineRegex(a, st, sp, t, ts, _) :: tail =>
          import Task.*
          val level    = a.length
          val state    = unnulifyAndStrip(st)
          val priority = unnulifyAndStrip(sp).flatMap(priorityStringToInt)
          val title    = unnulifyAndStrip(t).getOrElse("")
          val tags     = unnulifyAndStrip(ts).toList.flatMap(parseTags)
          val modifyTask = List(
            levelLense.replace(level),
            stateLense.replace(state),
            priorityLense.replace(priority),
            titleLense.replace(title),
            tagsLense.replace(tags)
          ).foldLeft(identity: Task => Task)((acc, f) => acc.andThen(f))
          ParserResult(
            modifyTask.valid,
            ParserInput(
              tail.mkString("\n"),
              Location.nextLine(newLocation)
            )
          )
        case _ => ParserResult.error(f"Unable to parse title string", s)
      }
    }
  }

  // TODO: handle different recurrence formats
  private def timeParser(name: String, separator: PairedSeparator, lense: Lens[Task, Option[ScheduledType]]) =
    new Parser[ScheduledType] {
      def parse(s: ParserInput) = {
        val regex                   = f"$name: ${separator.left}([^${separator.right}]*)${separator.right}(.*)".r
        val (lines, modifyLocation) = skipBlank(s.str)
        val newLocation             = modifyLocation(s.location)
        lines.split("\n").toList match {
          case l @ regex(time, left) :: rest =>
            ScheduledType.bestMatch(time) match {
              case Success(parsedTime) =>
                ParserResult(
                  parsedTime.valid,
                  ParserInput(
                    (left +: rest).mkString("\n"),
                    Location.modifyColumn(_ + (l.length - rest.length))(newLocation)
                  )
                )
              case Failure(e) => ParserResult.error(f"Unable to parse time string: $time", s)
            }
          case _ => ParserResult.error(f"Unable to parse time string", s)
        }
      }
    }.toNullary
      .lift[Task, Task]((f: Unit => ScheduledType) => lense.replace(f(()).some))
      .asIso

  private val propertyParser = new IsoParser[Task] {
    def parse(s: ParserInput) = {
      val regex                   = "^:([a-zA-Z0-9]+): (.*)$".r
      val (lines, modifyLocation) = skipBlank(s.str)
      val newLocation             = modifyLocation(s.location)
      lines.split("\n").toList match {
        case regex(k, v) :: rest =>
          ParserResult(
            Task.propertiesLense.modify(_ + (k -> v)).valid,
            ParserInput(
              rest.mkString("\n"),
              Location.nextLine(newLocation)
            )
          )
        case _ => ParserResult.error(f"Unable to parse property string", s)
      }
    }
  }

  private val bodyParser = new IsoParser[Task] {
    def parse(s: ParserInput) =
      s.str.split("\n").toList match {
        case Nil                                          => ParserResult.error("consumed everything", s)
        case head :: rest if head.isEmpty                 => ParserResult.error("consumed everything", s)
        case head :: rest if titleLineRegex.matches(head) => ParserResult.error("Start of the next task", s)
        case head :: rest =>
          ParserResult(
            Task.bodyLense.modify(a => if a.isEmpty then s"$head" else s"$a\n$head").valid,
            ParserInput(
              rest.mkString("\n"),
              Location.nextLine(s.location)
            )
          )
      }
  }

  private val dateTimeParser = IsoParser
    .makeParallel(
      timeParser("SCHEDULED", PairedSeparator.Triangles, Task.scheduledLense),
      timeParser("DEADLINE", PairedSeparator.Triangles, Task.deadlineLense),
      timeParser("CLOSED", PairedSeparator.Brackets, Task.closedLense),
    )

  private val taskParser = titleParser
    .andThen(dateTimeParser.repeatedWhileValid)
    .andThen(IsoParser.stringConsumer[Task](":PROPERTIES:"))
    .andThen(propertyParser.repeatedWhileValid)
    .andThen(IsoParser.stringConsumer[Task](":END:"))
    .andThen(bodyParser.repeatedWhileValid)
    .asIso

  private val orgFileTitleParser = new IsoParser[OrgFile] {
    def parse(s: ParserInput) = {
      val regex                   = "#\\+TITLE: (.*)$".r
      val (lines, modifyLocation) = skipBlank(s.str)
      val newLocation             = modifyLocation(s.location)
      lines.split("\n").toList match {
        case regex(title) :: rest =>
          ParserResult(
            OrgFile.titleLense.replace(title.some).valid,
            ParserInput(
              rest.mkString("\n"),
              Location.nextLine(newLocation)
            )
          )
        case _ => ParserResult.error(f"Unable to parse title string", s)
      }
    }
  }

  val forestTaskParser: TransformParser[Forest[Task], Forest[Task]] =
    taskParser
      .toListParser(Task())
      .repeatedWhileValid
      .liftValidated[Forest[Task], Forest[Task]](
        _.toList.map(_.value),
        l =>
          Forest.fromList(l, _.level) match {
            case Left(value)  => CodecError(value.toString).invalidNec
            case Right(value) => value.valid
          }
      )

  private val orgFileParser = orgFileTitleParser
    .andThen(
      forestTaskParser
        .lift[OrgFile, OrgFile](OrgFile.tasksLense.modify)
    )

  // ------------------ codecs ------------------------------

  given Codec[OrgFile] with {
    extension (s: String)
      override def decode: ValidatedCodec[OrgFile] =
        orgFileParser.parse(ParserInput.init(s))._1.map(_(OrgFile()))

    extension (a: OrgFile)
      override def encode: String =
        a.title
          .map(title => f"""|#+TITLE: ${title}
                            |
                            |${a.tasks.encode}
          """.stripMargin)
          .getOrElse(a.tasks.encode)
  }

  given Codec[Forest[Task]] with {
    extension (s: String)
      override def decode: ValidatedCodec[Forest[Task]] =
        forestTaskParser.parse(ParserInput.init(s))._1.map(_(Forest.empty))

    extension (a: Forest[Task])
      override def encode: String =
        a.toList.map(_.value.encode).mkString("\n")
  }

  given Codec[Task] with {
    extension (s: String) override def decode: ValidatedCodec[Task] = taskParser.parse(ParserInput.init(s))._1.map(_(Task()))

    extension (a: Task)
      override def encode: String = {
        val encodedTags: String =
          if (a.tags.isEmpty) "" else f":${a.tags.mkString(":")}:"

        def encodeScheduledType(
            name: String,
            start: String,
            end: String,
            s: Option[ScheduledType]
        ): Option[String] =
          s.map(x => f"$name: $start${x.encode}$end")

        val encodedTimeProperties = List(
          encodeScheduledType("CLOSED", "[", "]", a.closed),
          encodeScheduledType("DEADLINE", "<", ">", a.deadline),
          encodeScheduledType("SCHEDULED", "<", ">", a.scheduled)
        ).flatMap(_.toList).mkString(" ")

        val encodedPriority = a.priority
          .map { i =>
            val priorityLetter = ('A' + i).toChar
            f" [#$priorityLetter]"
          }
          .getOrElse("")

        val titleAndMeta =
          f"""|${"*" * a.level}${a.state.getOrElse("")}$encodedPriority ${a.title} $encodedTags
              |$encodedTimeProperties
              |${a.properties.encode}
        """.stripMargin
            .split("\n")
            .filter(_.nonEmpty)
            .mkString("\n")
            .trim()

        val body = f"${if (a.body.nonEmpty) f"\n${a.body}" else "\n"}"

        f"$titleAndMeta$body\n"
      }
  }

  given Codec[Properties] with {
    extension (s: String) override def decode: ValidatedCodec[Properties] = ???

    extension (a: Properties)
      override def encode: String =
        if (a.isEmpty) ""
        else
          f"""|:PROPERTIES:
              |${a.map { case (k, v) => f":$k: $v" }.mkString("\n")}
              |:END:
          """.stripMargin

  }

  given Codec[ScheduledType] with {
    extension (s: String) override def decode: ValidatedCodec[ScheduledType] = ???

    extension (a: ScheduledType)
      override def encode: String =
        DateTimeFormatter
          .ofPattern(
            a match {
              case _: LocalDate     => "yyyy-MM-dd EEE"
              case _: LocalDateTime => "yyyy-MM-dd EEE HH:mm"
            },
            Locale.ENGLISH
          )
          .format(a);
  }
}
