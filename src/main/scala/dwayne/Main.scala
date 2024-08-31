package dwayne

// import dwayne.parser.*
// import dwayne.org.*
import zio.Console
// import zio.ZIO
import zio.ZIOAppDefault
// import dwayne.tui.AllTasks
import dwayne.tui.EditInTextEditor

object Main extends ZIOAppDefault {

  val taskStr = """
    |* TODO [#A] Some title :some:tags2:
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    | something
    |** TODO [#A] Some title :some:tags2:
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    |*** TODO [#A] Some title :some:tags2:
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    |** TODO [#A] Some title :some:tags2:
    |  :PROPERTIES:
    |  :prop1: value1
    |  :prop2: value2
    |  :END:
    |  Some body
    |""".stripMargin.trim

  def run = for {
    lines <- EditInTextEditor.editInTextEditor(taskStr)
    _ <- Console.printLine(lines)
    // _ <- ZIO.succeed(AllTasks.demo())
    // tasks <- ZIO.succeed(OrgParser.taskForest.parse(PInput(Text(taskStr), Location(0, 0))))
    // _ <- Console.printLine(tasks.toOption.get.result)
  } yield ()
}
