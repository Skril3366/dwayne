package dwayne

// import cats.data.NonEmptyList
// import cats.data.Validated
// import cats.data.Validated.Invalid
// import cats.data.Validated.Valid
import cats.syntax.all.*
import dwayne.codec.asIso
// import dwayne.codec.Codec
// import dwayne.codec.CodecError
import dwayne.codec.IsoStringTransformer
// import dwayne.codec.Location
import dwayne.codec.StringTransformer
import dwayne.codec.ValidatedCodec
// import dwayne.data.Forest
// import dwayne.data.TreeNodeWithLevel
// import dwayne.org.Properties
// import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime
import monocle.Lens
// import java.util.Locale
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

  //  ------------------ transformers -------------------------

  private val titleTransformer = new IsoStringTransformer[Task] {
    def consume(s: String) = s.strip.split("\n").toList match {
      case titleLineRegex(a, s, sp, t, ts, _) :: tail =>
        import Task.*
        val level    = a.length
        val state    = unnulifyAndStrip(s)
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
        (modifyTask.valid, tail.mkString("\n"))
      case _ => error(s, f"Unable to parse title string: $s")
    }
  }

  private def timeTransformer(name: String, separator: PairedSeparator, lense: Lens[Task, Option[ScheduledType]]) =
    new StringTransformer[Unit, ScheduledType] {
      def consume(s: String) = {
        val regex = f"$name: ${separator.left}([^${separator.right}]*)${separator.right}(.*)".r
        s.strip.split("\n").toList match {
          case regex(time, left) :: rest =>
            ScheduledType.bestMatch(time) match {
              case Success(parsedTime) => (((_: Unit) => parsedTime).valid, (left +: rest).mkString("\n"))
              case Failure(e)          => error(s, f"Unable to parse time string: $time")
            }
          case _ => error(s, f"Unable to parse time string: $s")
        }
      }
    }
      .lift[Task, Task](_ => (), b => lense.replace(b.some))
      .asIso

  private val propertyTransformer = new IsoStringTransformer[Task] {
    def consume(s: String) = {
      val regex = "^:([a-zA-Z0-9]+): (.*)$".r
      s.strip.split("\n").toList match {
        case regex(k, v) :: rest =>
          (Task.propertiesLense.modify(_ + (k -> v)).valid, rest.mkString("\n"))
        case _ => error(s, f"Unable to parse property string: $s")
      }
    }
  }

  private val bodyTransformer = new IsoStringTransformer[Task] {
    def consume(s: String) =
      s.split("\n").toList match {
        case Nil                                          => error(s, "consumed everything")
        case head :: rest if head.isEmpty                 => error(s, "consumed everything")
        case head :: rest if titleLineRegex.matches(head) => error(s, "Start of the next task")
        case head :: rest =>
          (Task.bodyLense.modify(a => if a.isEmpty then s"$head" else s"$a\n$head").valid, rest.mkString("\n"))
      }
  }

  private val dateTimeTransformer = IsoStringTransformer
    .makeParallel(
      timeTransformer("SCHEDULED", PairedSeparator.Triangles, Task.scheduledLense),
      timeTransformer("DEADLINE", PairedSeparator.Triangles, Task.deadlineLense),
      timeTransformer("CLOSED", PairedSeparator.Brackets, Task.closedLense),
    )

  private val taskTransformer = titleTransformer
    .andThen(dateTimeTransformer.repeatedWhileValid)
    .andThen(IsoStringTransformer.stringConsumer[Task](":PROPERTIES:"))
    .andThen(propertyTransformer.repeatedWhileValid)
    .andThen(IsoStringTransformer.stringConsumer[Task](":END:"))
    .andThen(bodyTransformer.repeatedWhileValid)
    .asIso

  private val orgFileTitleTransformer = new IsoStringTransformer[OrgFile] {
    def consume(s: String): (ValidatedCodec[OrgFile => OrgFile], String) = {
      val regex = "#\\+TITLE: (.*)".r
      s.strip.split("\n").toList match {
        case regex(title) :: rest =>
          (OrgFile.titleLense.replace(title.some).valid, rest.mkString("\n"))
        case _ => error(s, f"Unable to parse title string: $s")
      }
    }
  }

  private val listOfTasksTransformer =
    taskTransformer.toListTransformer.repeatedWhileValid
      .lift[OrgFile, OrgFile](
        _.tasks.map(t => (_: Task) => t),
        l => OrgFile.tasksLense.replace(l.map(_(Task())))
      )

  val orgFileTransformer = orgFileTitleTransformer.andThen(listOfTasksTransformer)

  // ------------------ codecs ------------------------------

  // given Codec[OrgFile] with {
  //   extension (s: String)
  //     override def decode: ValidatedCodec[OrgFile] =
  //       s.split("\n").toList match {
  //         case List() => OrgFile(None, Forest.empty).valid
  //         case title :: tasks if title.matches("#\\+TITLE:.*") =>
  //           val titleStr = title.replace("#+TITLE: ", "").trim()
  //           val tasksForest: ValidatedCodec[Forest[Task]] =
  //             tasks.mkString("\n").decode
  //           tasksForest.map(OrgFile(titleStr.some, _))
  //         case _ =>
  //           ValidatedCodec.error(
  //             f"Unexpected error with: $s",
  //             Location(0).some
  //           )
  //       }
  //
  //   extension (a: OrgFile)
  //     override def encode: String =
  //       a.title
  //         .map(title => f"""|#+TITLE: ${title}
  //                           |
  //                           |${a.tasks.encode}
  //         """.stripMargin)
  //         .getOrElse(a.tasks.encode)
  // }
  //
  // given Codec[Forest[Task]] with {
  //   extension (s: String)
  //     override def decode: ValidatedCodec[Forest[Task]] = {
  //       val linesGrouppedByTasks: List[List[String]] =
  //         s.split("\n")
  //           .toList
  //           .dropWhile(_.isEmpty)
  //           .foldLeft(List.empty[List[String]])((groups, line) =>
  //             (titleLineRegex.matches(line), groups.isEmpty) match {
  //               case (true, true)  => List(List(line))
  //               case (true, false) => groups :+ List(line)
  //               case _             => groups.init :+ (groups.last :+ line)
  //             }
  //           )
  //
  //       val tasks: List[ValidatedCodec[TreeNodeWithLevel[Task]]] =
  //         linesGrouppedByTasks.map(
  //           _.mkString("\n").decode: ValidatedCodec[TreeNodeWithLevel[Task]]
  //         )
  //
  //       tasks.map(_.map(List(_))) match {
  //         case Nil => Forest.empty.valid
  //         case head :: tail =>
  //           tail.fold(head)((a, b) => a combine b) match {
  //             case i @ Invalid(_) => i
  //             case Valid(a) =>
  //               Forest
  //                 .fromNodes(a)
  //                 .leftMap(e => NonEmptyList.one(CodecError(e.toString)))
  //                 .toValidated
  //           }
  //       }
  //     }
  //
  //   extension (a: Forest[Task])
  //     override def encode: String =
  //       a.toList.map(_.encode).mkString("\n")
  // }
  //
  // given Codec[TreeNodeWithLevel[Task]] with {
  //   extension (s: String)
  //     override def decode: ValidatedCodec[TreeNodeWithLevel[Task]] =
  //       (s.decode: ValidatedCodec[Task])
  //         .map(t =>
  //           TreeNodeWithLevel(
  //             s.takeWhile(_ == '*').length - 1 + Forest.rootLevel, // TODO: understand how to make this calculation more generic
  //             t
  //           )
  //         )
  //
  //   extension (a: TreeNodeWithLevel[Task])
  //     override def encode: String =
  //       f"${"*" * (a.level + Forest.rootLevel + 1)} ${a.value.encode}" // TODO: understand how to make this calculation more generic
  //
  // }
  //
  // given Codec[Task] with {
  //
  //   extension (s: String)
  //     override def decode: ValidatedCodec[Task] = {
  //
  //       val lines = s.split("\n").toList
  //
  //       def taskWithParcedTitle(titleLine: String): ValidatedCodec[Task] =
  //         titleLine match {
  //           case titleLineRegex(s, sp, t, ts, _) =>
  //             def unnulifyAndStrip(s: String): Option[String] =
  //               Option(s).map(_.strip).filter(_.nonEmpty)
  //             Task(
  //               state = unnulifyAndStrip(s),
  //               title = unnulifyAndStrip(t).getOrElse(""),
  //               priority = unnulifyAndStrip(sp)
  //                 .flatMap(priorityStringToInt(_)),
  //               tags = unnulifyAndStrip(ts).toList.flatMap(
  //                 _.split(":").toList.filter(_.nonEmpty)
  //               )
  //             ).valid
  //           case _ =>
  //             ValidatedCodec.error(
  //               f"Unable to parse title string: $titleLine"
  //             )
  //         }
  //
  //       // TODO: parse task contents
  //       lines.headOption
  //         .map(taskWithParcedTitle)
  //         .getOrElse(
  //           ValidatedCodec.error(
  //             "No lines provided to task parser"
  //           )
  //         )
  //     }
  //
  //   extension (a: Task)
  //     override def encode: String = {
  //       val encodedTags: String =
  //         if (a.tags.isEmpty) "" else f":${a.tags.mkString(":")}:"
  //
  //       def encodeScheduledType(
  //           name: String,
  //           start: String,
  //           end: String,
  //           s: Option[ScheduledType]
  //       ): Option[String] =
  //         s.map(x => f"$name: $start${x.encode}$end")
  //
  //       val encodedTimeProperties = List(
  //         encodeScheduledType("CLOSED", "[", "]", a.closed),
  //         encodeScheduledType("DEADLINE", "<", ">", a.deadline),
  //         encodeScheduledType("SCHEDULED", "<", ">", a.scheduled)
  //       ).flatMap(_.toList).mkString(" ")
  //
  //       val encodedPriority = a.priority
  //         .map { i =>
  //           val priorityLetter = ('A' + i).toChar
  //           f" [#$priorityLetter]"
  //         }
  //         .getOrElse("")
  //
  //       val titleAndMeta =
  //         f"""|${a.state.getOrElse("")}$encodedPriority ${a.title} $encodedTags
  //             |$encodedTimeProperties
  //             |${a.properties.encode}
  //       """.stripMargin
  //           .split("\n")
  //           .filter(_.nonEmpty)
  //           .mkString("\n")
  //           .trim()
  //
  //       val body = f"${if (a.body.nonEmpty) f"\n${a.body}" else "\n"}"
  //
  //       f"$titleAndMeta$body\n"
  //     }
  // }
  //
  // given Codec[Properties] with {
  //   extension (s: String) override def decode: ValidatedCodec[Properties] = ???
  //
  //   extension (a: Properties)
  //     override def encode: String =
  //       if (a.isEmpty) ""
  //       else
  //         f"""|:PROPERTIES:
  //             |${a.map { case (k, v) => f":$k: $v" }.mkString("\n")}
  //             |:END:
  //         """.stripMargin
  //
  // }
  //
  // given Codec[ScheduledType] with {
  //   extension (s: String) override def decode: ValidatedCodec[ScheduledType] = ???
  //
  //   extension (a: ScheduledType)
  //     override def encode: String =
  //       DateTimeFormatter
  //         .ofPattern(
  //           a match {
  //             case _: LocalDate     => "yyyy-MM-dd EEE"
  //             case _: LocalDateTime => "yyyy-MM-dd EEE HH:mm"
  //           },
  //           Locale.ENGLISH
  //         )
  //         .format(a);
  // }
}
