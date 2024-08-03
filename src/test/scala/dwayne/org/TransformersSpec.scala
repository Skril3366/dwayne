package dwayne.org

import cats.syntax.all.*
import java.time.LocalDate
import java.time.LocalDateTime
import zio.{Task => _, _}
import zio.test._

object TransformersSpec extends ZIOSpecDefault {

  def spec =
    suite("TransformersSpec")(
      test("Should parse org mode time format correctly") {
        List(
          ("2023-09-02 Sat 12:01", LocalDateTime.of(2023, 9, 2, 12, 1)),
          ("2023-09-02 Sat", LocalDate.of(2023, 9, 2))
        ).foldLeft(assertTrue(true)) { case (acc, (s, expected)) =>
          acc && assertTrue(
            ScheduledType.bestMatch(s).toOption.get == expected
          )
        }
      },
      test("Should format org mode time correctly") {
        List[(ScheduledType, String)](
          (LocalDateTime.of(2023, 9, 2, 12, 1), "2023-09-02 Sat 12:01"),
          (LocalDate.of(2023, 9, 2), "2023-09-02 Sat")
        ).foldLeft(assertTrue(true)) { case (acc, (s, expected)) =>
          acc && assertTrue(
            ScheduledType.format(s) == expected
          )
        }
      },
      test("Should parse title line correctly") {
        // TODO: make more generalised as in OrgCodecSpec
        val titleLine      = "* TODO [#A] Some title :some:tags2:"
        val (parsed, rest) = titleTransformer.consume(titleLine)
        val task           = parsed.toOption.get(Task())
        assertTrue(
          Task(
            state = "TODO".some,
            priority = 0.some,
            title = "Some title",
            tags = List("some", "tags2")
          ) == task
        ) && assertTrue(rest.isEmpty)
      },
      test("Should correctly parse full task"){
        val taskStr = """
          |* TODO [#A] Some title :some:tags2:
          |  SCHEDULED: <2023-09-02 Sat>
          |  DEADLINE: <2023-09-02 Sat 12:01>
          |  CLOSED: <2023-09-02 Sat 12:01>
          |  :PROPERTIES:
          |  :prop1: value1
          |  :prop2: value2
          |  :END:
          |  Some body
          |""".stripMargin
        val (parsed, rest) = taskTransformer.consume(taskStr)
        val task = parsed.toOption.get(Task())
        assertTrue(
          Task(
            state = "TODO".some,
            priority = 0.some,
            title = "Some title",
            tags = List("some", "tags2"),
            scheduled = LocalDate.of(2023, 9, 2).some,
            deadline = LocalDateTime.of(2023, 9, 2, 12, 1).some,
            closed = LocalDateTime.of(2023, 9, 2, 12, 1).some,
            properties = Map("prop1" -> "value1", "prop2" -> "value2"),
            body = "Some body"
          ) == task
        ) && assertTrue(rest.isEmpty)
      }
    )
}
