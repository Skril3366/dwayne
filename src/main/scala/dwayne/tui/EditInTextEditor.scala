package dwayne.tui

import java.io.File
import java.io.FileWriter
import java.lang.ProcessBuilder
import scala.io.Source
import zio.Scope
import zio.Task
import zio.ZIO

object EditInTextEditor {
  def editInTextEditor(input: String): ZIO[Scope, Throwable, String] = for {
    file     <- makeTempFileWith()
    _        <- writeToFile(file, input)
    editor   <- ZIO.attempt(System.getenv().getOrDefault("EDITOR", "vim"))
    exitCode <- runProcessInForeground(s"$editor ${file.getAbsolutePath}")
    output   <- readFromFile(file)
  } yield output

  private def runProcessInForeground(command: String): Task[Int] = ZIO.attemptBlocking(
    new ProcessBuilder(command.split(" "): _*).inheritIO().start().waitFor()
  )

  private def makeTempFileWith(): ZIO[Scope, Throwable, File] =
    ZIO.acquireRelease(
      ZIO
        .attempt(File.createTempFile("temp", ".org"))
    )(f =>
      ZIO
        .attempt(f.delete())
        .ignore
    )

  private def readFromFile(file: File): ZIO[Scope, Throwable, String] =
    ZIO.attemptBlocking(Source.fromFile(file).getLines().mkString("\n"))

  private def writeToFile(file: File, content: String): ZIO[Scope, Throwable, Unit] =
    ZIO.acquireReleaseWith(ZIO.attempt(new FileWriter(file, false)))(fileWriter =>
      ZIO
        .attempt(fileWriter.close())
        .ignore
    )(writer => ZIO.attempt(writer.write(content)))
}
