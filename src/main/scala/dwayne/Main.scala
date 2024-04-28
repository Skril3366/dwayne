package dwayne

import java.io.DataInputStream
import java.io.FileNotFoundException
import java.nio.charset.StandardCharsets
import scala.io.Source
import scala.util.Using._
import zio.*

trait AppExeption extends Throwable {
  def message: String
}

case class FileNotFound(fileName: String) extends AppExeption {
  override def message: String = s"File not found: $fileName"
}

case class UnexpectedError(cause: Throwable) extends AppExeption {
  override def message: String =
    s"Unexpected error: ${Option(cause.getMessage).getOrElse("No message")}"
}

object FileReader {
  def readResource(
      fileName: String
  ): ZIO[Scope, FileNotFound | UnexpectedError, String] = ZIO
    .acquireRelease(ZIO.attempt(Source.fromFile(fileName)))(f =>
      ZIO.succeed(f.close())
    )
    .map(_.getLines().mkString)
    .catchAll {
      case e: FileNotFoundException => ZIO.fail(FileNotFound(fileName))
      case e                        => ZIO.fail(UnexpectedError(e))
    }
}

object Main extends ZIOAppDefault {

  def x: ZIO[Scope, UnexpectedError, String] =
    FileReader.readResource("Tasks.org").catchAll {
      case e: FileNotFound    => ZIO.succeed(s"File not found: ${e.fileName}")
      case e: UnexpectedError => ZIO.fail(e)
    }

  def run = for {
    lines <- x
    _ <- Console.printLine(lines)
  } yield ()
}
