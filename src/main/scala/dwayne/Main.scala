package dwayne

import dwayne.org.*
import dwayne.codec.ParserInput
import dwayne.codec.applyTo
// import dwayne.org.given_Codec_OrgFile
import zio.Console
import zio.ZIO
import zio.ZIOAppDefault
import dwayne.data.Forest

object Main extends ZIOAppDefault {

  val taskStr = """
    |* TODO [#A] Some title :some:tags2:
    |  SCHEDULED: <2023-09-02 Sat> DEADLINE: <2023-09-02 Sat 12:01> CLOSED: [2023-09-02 Sat 12:01]
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    |** TODO [#A] Task 2
    |   CLOSED: [2023-09-02 Sat 12:01] DEADLINE: <2023-09-02 Sat 12:01>
    |  :PROPERTIES:
    |  :PROP: VALUE
    |  :END:
    |
    |
    |
    |  SOME BODY 2
    |  SOME BODY 2
    |
    |
    |
    |
    |
    |  SOME BODY 2
    |""".stripMargin

  val scheduled = "SCHEDULED: <2023-09-02 Sat>"

  def run = for {
    // t <- ZIO.attempt(orgFileParser.consume(taskStr))
    // (makes, rest) = t
    // results = makes.map(_(OrgFile())).getOrElse(OrgFile())
    // _ <- ZIO.attempt(println(results))
    // _ <- ZIO.attempt(println(s"'$rest'"))
    // _ <- ZIO.attempt(println(s"'${Nil.mkString("\n")}'"))

    // args <- getArgs
    // _ <- Console.printLine(args.mkString(", "))
    // _ <- Console.printLine(System.getProperty("user.dir"))
    file <- FileReader
      .readFile("./src/main/resources/Tasks.org")
      // .readFile("./src/main/resources/Phone.org")
      .catchAll {
        case e: FileNotFound    => ZIO.succeed(s"File not found: ${e.fileName}")
        case e: UnexpectedError => ZIO.fail(e)
      }
    parsed = forestTaskParser.parse(ParserInput.init(taskStr)).applyTo(Forest.empty).result
    _ <- Console.printLine(parsed)

  } yield ()
}
