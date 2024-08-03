package dwayne.org

import dwayne.data.Forest
import java.time.format.DateTimeFormatter
import java.time.LocalDate
import java.time.LocalDateTime
import java.util.Locale
import monocle.macros.GenLens
import monocle.Lens
import scala.util.Try

type ScheduledType = LocalDate | LocalDateTime

object ScheduledType {
  val localDatePattern     = "yyyy-MM-dd EEE"
  val localDateTimePattern = "yyyy-MM-dd EEE HH:mm"

  def lense(s: ScheduledType): ScheduledType => ScheduledType = _ => s

  def bestMatch(s: String): Try[ScheduledType] = {
    val dateTime = Try(LocalDateTime.parse(s, DateTimeFormatter.ofPattern(localDateTimePattern, Locale.ENGLISH)))
    val date     = Try(LocalDate.parse(s, DateTimeFormatter.ofPattern(localDatePattern, Locale.ENGLISH)))
    dateTime.orElse(date)
  }

  def format(a: ScheduledType) = DateTimeFormatter
    .ofPattern(
      a match {
        case _: LocalDate     => localDatePattern
        case _: LocalDateTime => localDateTimePattern
      },
      Locale.ENGLISH
    )
    .format(a);
}

case class OrgFile(
    title: Option[String],
    tasks: Forest[Task]
)

case class Task(
    level: Int = 0,
    state: Option[String] = None,
    priority: Option[Int] = None,
    title: String = "",
    tags: List[String] = List(),
    scheduled: Option[ScheduledType] = None,
    deadline: Option[ScheduledType] = None,
    closed: Option[ScheduledType] = None,
    properties: Properties = Map(),
    body: String = ""
)

object Task {
  val levelLense      = GenLens[Task](_.level)
  val stateLense      = GenLens[Task](_.state)
  val priorityLense   = GenLens[Task](_.priority)
  val titleLense      = GenLens[Task](_.title)
  val tagsLense       = GenLens[Task](_.tags)
  val scheduledLense  = GenLens[Task](_.scheduled)
  val deadlineLense   = GenLens[Task](_.deadline)
  val closedLense     = GenLens[Task](_.closed)
  val propertiesLense = GenLens[Task](_.properties)
  val bodyLense       = GenLens[Task](_.body)
}

type Properties = Map[String, String]
