package dwayne

import java.io.FileNotFoundException
import scala.io.Source
import zio.*

object FileReader {
  def readFile(
      fileName: String
  ): ZIO[Scope, FileNotFound | UnexpectedError, String] = ZIO
    .acquireRelease(ZIO.attempt(Source.fromFile(fileName)))(f =>
      ZIO.succeed(f.close())
    )
    .map(_.getLines().mkString("\n"))
    .catchAll {
      case e: FileNotFoundException => ZIO.fail(FileNotFound(fileName))
      case e                        => ZIO.fail(UnexpectedError(e))
    }
}
