package dwayne

import dwayne.codec.*
import dwayne.data.*
import dwayne.org.*
import dwayne.org.given
import java.time.Instant
import java.time.LocalDate
import java.time.LocalDateTime
import zio.Console
import zio.Scope
import zio.ZIO
import zio.ZIOAppDefault
import cats.syntax.all.*

object Main extends ZIOAppDefault {

  val testFile = OrgFile(
    title = "Test".some,
    tasks = Tree.Node(
      Task(
        state = "TODO",
        title = "Test",
        tags = List("test", "another", "tag"),
        properties = Map("TEST_PROP" -> "TEST_VAL"),
        priority = 2.some,
        scheduled = LocalDateTime.now.some,
        deadline = LocalDate.now.some,
        closed = LocalDateTime.now.some,
        body = "Test\nTest\nTest"
      ),
      List(
        Tree.Leaf(
          Task(
            state = "INBOX",
            title = "Test",
            tags = List(),
            properties = Map(),
            priority = None,
            scheduled = None,
            deadline = None,
            closed = None,
            body = "Test"
          )
        ),
        Tree.Leaf(
          Task(
            state = "TODO",
            title = "Test",
            tags = List(),
            properties = Map("test" -> "test"),
            priority = None,
            scheduled = None,
            deadline = None,
            closed = None,
            body = "Test"
          )
        )
      )
    )
  )

  def run = for {
    // args <- getArgs
    // _ <- Console.printLine(args.mkString(", "))
    // _ <- Console.printLine(System.getProperty("user.dir"))
    // lines <- FileReader
    //   .readFile("./src/main/resources/Tasks.org")
    //   .catchAll {
    //     case e: FileNotFound    => ZIO.succeed(s"File not found: ${e.fileName}")
    //     case e: UnexpectedError => ZIO.fail(e)
    //   }
    // parsed = OrgFile.parse(lines)
    // _ <- Console.printLine(
    //   if parsed.isValid then "Valid"
    //   else "Errors"
    // )

    _ <- Console.printLine(
      testFile.encode
    )
  } yield ()
}
