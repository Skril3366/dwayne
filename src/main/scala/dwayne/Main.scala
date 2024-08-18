package dwayne

import dwayne.parser.*
import zio.Console
import zio.ZIO
import zio.ZIOAppDefault

object Main extends ZIOAppDefault {

  val taskStr = """
    |* TODO [#A] Some title :some:tags2:
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    | something
    |* TODO [#A] Some title :some:tags2:
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    |""".stripMargin

  def run = for {
    tasks <- ZIO.succeed(OrgParser.taskForest.parse(PInput(Text(taskStr), Location(0, 0))))
    _ <- Console.printLine(tasks)
  } yield ()
}
