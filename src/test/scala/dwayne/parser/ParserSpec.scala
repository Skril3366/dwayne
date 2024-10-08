package dwayne.parser

import cats.syntax.all.*
import scala.util.Try
import zio.{Task => _, _}
import zio.test._

object TransformersSpec extends ZIOSpecDefault {

  val digitParser = new Parser[Option, Int] {
    def parse(in: PInput): Option[PResult[Int]] =
      in.text.lines match
        case head :: next =>
          head.headOption.flatMap { c =>
            Try(Integer.parseInt(c.toString)).toOption.map(digit =>
              PResult(digit, PInput(Text(List(head.tail).appendedAll(next)), in.location.modifyColumn(_ + 1)))
            )
          }
        case Nil => None
  }

  def wordParser(str: String) = new Parser[Option, String] {
    def parse(in: PInput): Option[PResult[String]] = in.text.lines match {
      case head :: next =>
        head.split(" ").toList match {
          case str :: rest => Some(PResult(str, PInput(Text(rest.mkString(" ")), in.location)))
          case Nil         => None
        }
      case Nil => None
    }

  }

  def spec =
    suite("Parser")(
      test("Parallel function should work correctly") {
        val input = "SCHEDULED DEADLINE CLOSED"
        val parallel = Parser.parallel[Option, String](
          _.isDefined,
          input.split(" ").map(wordParser).toList
        )
        val res = parallel.parse(PInput(Text(input), Location(0, 0))).value.flatten

        assertTrue(res.isDefined) &&
        assertTrue(res.get.result == "SCHEDULED")
      },
      test("Monadic composition should work correctly") {
        val manual = for {
          one   <- digitParser
          two   <- digitParser
          three <- digitParser
          four  <- digitParser
        } yield List(one, two, three, four)

        val input  = "1234"
        val parsed = manual.parse(PInput(Text(input), Location(0, 0)))

        assertTrue(parsed.isDefined) &&
        assertTrue(parsed.get.result == List(1, 2, 3, 4))
      },
      test("Monadic composition should fail correctly") {
        val manual = for {
          one   <- digitParser
          two   <- digitParser
          three <- digitParser
          four  <- digitParser
        } yield List(one, two, three, four)

        val input  = "1a34"
        val parsed = manual.parse(PInput(Text(input), Location(0, 0)))

        assertTrue(parsed.isEmpty)
      },
      test("repeatUntil should work correctly") {
        val l        = (1 to 9).toList
        val repeated = digitParser.repeatedUntil(_.isEmpty)
        val res      = repeated.parse(PInput(Text(l.mkString), Location(0, 0)))

        assertTrue(res.isDefined) &&
        assertTrue(res.get.result == l)
      }
    )
}
