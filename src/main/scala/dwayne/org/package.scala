package dwayne

import cats.syntax.all.*
import dwayne.codec.*
import dwayne.data.Forest
import dwayne.org.Properties

enum PairedSeparator(val left: String, val right: String) {
  case Brackets extends PairedSeparator(left = "\\[", right = "\\]")
  case Triangles extends PairedSeparator(left = "<", right = ">")
}

package org {
  // ------------------ codecs ------------------------------

  given Encoder[OrgFile] with {
    override def encode(a: OrgFile): String =
      a.title
        .map(title => f"""|#+TITLE: ${title}
                          |
                          |${a.tasks.encode}
          """.stripMargin)
        .getOrElse(a.tasks.encode)
  }

  given Encoder[Forest[Task]] with {
    override def encode(a: Forest[Task]): String =
      a.toList.map(_.value.encode).mkString("\n")
  }

  given Encoder[Task] with {
    override def encode(a: Task): String = {
      val encodedTags: String =
        if (a.tags.isEmpty) "" else f":${a.tags.mkString(":")}:"

      def encodeScheduledType(
          name: String,
          start: String,
          end: String,
          s: Option[ScheduledType]
      ): Option[String] =
        s.map(x => f"$name: $start${x.encode}$end")

      val encodedTimeProperties =
        List(
        ("CLOSED", "[", "]", a.closed),
        ("DEADLINE", "<", ">", a.deadline),
        ("SCHEDULED", "<", ">", a.scheduled)
      )
        .map(
        encodeScheduledType
      ).flatMap(_.toList)
        .mkString(" ")

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

  given Encoder[Properties] with {
    override def encode(a: Properties): String =
      if (a.isEmpty) ""
      else
        f"""|:PROPERTIES:
            |${a.map { case (k, v) => f":$k: $v" }.mkString("\n")}
            |:END:
          """.stripMargin

  }

  given Encoder[ScheduledType] with {
    override def encode(a: ScheduledType): String = ScheduledType.format(a)
  }
}
