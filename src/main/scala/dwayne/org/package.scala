package dwayne

import cats.data.Validated
import dwayne.codec.Codec
import dwayne.codec.CodecError
import dwayne.data.Tree
import dwayne.data.TreeNode
import dwayne.org.OrgFile
import dwayne.org.Properties
import dwayne.org.Task
import java.time.format.DateTimeFormatter
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import java.time.ZoneId
import java.util.Locale

package org {
  given Codec[OrgFile] with {
    extension (s: String)
      override def decode: Validated[OrgFile, CodecError] = ???

    extension (a: OrgFile)
      override def encode: String =
        a.title
          .map(title => f"""|#+TITLE: ${title}
                            |
                            |${a.tasks.encode}
          """.stripMargin)
          .getOrElse(a.tasks.encode)
  }

  given Codec[Tree[Task]] with {
    extension (s: String)
      override def decode: Validated[Tree[Task], CodecError] = ???

    extension (a: Tree[Task])
      override def encode: String =
        a.toList.map(_.encode).mkString("\n")
  }

  given Codec[TreeNode[Task]] with {
    extension (s: String)
      override def decode: Validated[TreeNode[Task], CodecError] = ???

    extension (a: TreeNode[Task])
      override def encode: String = f"${"*" * a.level} ${a.value.encode}"
  }

  given Codec[Task] with {

    extension (s: String) override def decode: Validated[Task, CodecError] = ???

    extension (a: Task)
      override def encode: String = {
        val encodedTags: String =
          if (a.tags.isEmpty) "" else a.tags.fold(":")((a, b) => f"$a$b:")

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
          f"""${a.state}$encodedPriority ${a.title} $encodedTags
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
    extension (s: String)
      override def decode: Validated[Properties, CodecError] = ???

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
    extension (s: String)
      override def decode: Validated[ScheduledType, CodecError] = ???

    extension (a: ScheduledType)
      override def encode: String = {
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
}
