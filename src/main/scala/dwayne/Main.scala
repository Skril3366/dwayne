package dwayne

import cats.syntax.all.*
// import dwayne.codec.*
import dwayne.data.*
import dwayne.org.*
import java.time.LocalDate
import java.time.LocalDateTime
import zio.ZIO
import zio.ZIOAppDefault

object Main extends ZIOAppDefault {

  val testFile = OrgFile(
    title = "Test".some,
    tasks = Forest(
      List(
        TreeNode(
          Task(
            state = "TODO".some,
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
            TreeNode(
              Task(
                state = "INBOX".some,
                title = "Test",
                tags = List(),
                properties = Map(),
                priority = None,
                scheduled = None,
                deadline = None,
                closed = None,
                body = "Test"
              ),
              List(
                TreeNode(
                  Task(
                    state = "Someday".some,
                    title = "Someday I will do that",
                    tags = List(),
                    properties = Map(),
                    priority = None,
                    scheduled = None,
                    deadline = None,
                    closed = None,
                    body = "Test Body"
                  )
                )
              )
            ),
            TreeNode(
              Task(
                state = "TODO".some,
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
    )
  )

  val taskStr = """
    |* TODO [#A] Some title :some:tags2:
    |  SCHEDULED: <2023-09-02 Sat> DEADLINE: <2023-09-02 Sat 12:01> CLOSED: [2023-09-02 Sat 12:01]
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    |""".stripMargin

  val scheduled = "SCHEDULED: <2023-09-02 Sat>"

  def run = for {
    t <- ZIO.attempt( taskTransformer.consume(taskStr))
    (make, rest) = t
    _ <- ZIO.attempt(println(make.map(_(Task()))))
    _ <- ZIO.attempt(println(s"'$rest'"))

    // args <- getArgs
    // _ <- Console.printLine(args.mkString(", "))
    // _ <- Console.printLine(System.getProperty("user.dir"))
    // file <- FileReader
    //   .readFile("./src/main/resources/Tasks.org")
    //   .catchAll {
    //     case e: FileNotFound    => ZIO.succeed(s"File not found: ${e.fileName}")
    //     case e: UnexpectedError => ZIO.fail(e)
    //   }
    // parsed: ValidatedCodec[OrgFile] = file.decode
    // _ <- Console.printLine(
    //   parsed.toEither.map(_.encode)
    // )
    // _ <- Console.printLine(testFile.encode)
    // _ <- Console.printLine(Forest.fromNodes(testFile.tasks.toList))
    // _ <- Console.printLine(testFile.tasks)
    // _ <- Console.printLine(
    //   Forest.fromNodes(testFile.tasks.toList) == testFile.tasks
    // )

  } yield ()
}
