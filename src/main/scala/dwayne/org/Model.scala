package dwayne.org

import cats.data.Validated
import dwayne.codec.Codec
import dwayne.data.Tree
import java.time.LocalDate
import java.time.LocalDateTime

type ScheduledType = LocalDate | LocalDateTime

case class OrgFile(
    title: Option[String],
    tasks: Tree[Task]
)

case class Task(
    state: String,
    title: String,
    tags: List[String],
    priority: Option[Int],
    scheduled: Option[ScheduledType],
    deadline: Option[ScheduledType],
    closed: Option[ScheduledType],
    properties: Properties,
    body: String
)

type Properties = Map[String, String]
