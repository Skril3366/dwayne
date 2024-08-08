package dwayne.codec

import cats.data.NonEmptyChain
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
import cats.Functor
import monocle.macros.GenLens
import scala.annotation.tailrec
import scala.util.Try

// TODO: try to remove try-catch hack :)
case class InternalException(errors: NonEmptyChain[CodecError])
    extends IllegalArgumentException("Used to pass the error up")

case class ParserInput(
    str: String,
    location: Location // should point and the beginning of the current str in the input
)

object ParserInput {
  def init(s: String) = ParserInput(s, Location(0, 0))
}

case class ParserResult[T](
    result: ValidatedCodec[T],
    leftOver: ParserInput
) {
  def isValid = result.isValid

  def mapResult[O](f: ValidatedCodec[T] => ValidatedCodec[O]): ParserResult[O] =
    ParserResult(f(result), leftOver)
  def map[O](f: T => O): ParserResult[O] =
    ParserResult(result.map(f), leftOver)
}

object ParserResult {
  final def error[T](
      message: String,
      leftOver: ParserInput,
  ) =
    ParserResult[T](
      CodecError(message, leftOver.location.some).invalidNec,
      leftOver
    )

  def resultLense[T]   = GenLens[ParserResult[T]](_.result)
  def leftOverLense[T] = GenLens[ParserResult[T]](_.leftOver)
}

// TODO: try to remove try-catch hack :)
extension [I, O](pr: ParserResult[I => O])
  def applyTo(i: I): ParserResult[O] =
    Try(pr.mapResult(f => f.map(_(i)))).toEither
      .fold(
        e =>
          e match {
            case e: InternalException => ParserResult.error(e.errors.toString, pr.leftOver)
            case _                    => ParserResult.error(e.getMessage, pr.leftOver)
          },
        identity
      )

trait Parser[T] {
  def parse(s: ParserInput): ParserResult[T]
  def toNullary = {
    def oldParse = parse
    new TransformParser[Unit, T] {
      def parse(s: ParserInput) = oldParse(s).map(f => (_: Unit) => f)
    }
  }
}

// given string s this parser produces a function that transforms I into O (you can think of it like a prism + a codec)
trait TransformParser[I, O] extends Parser[I => O] {
  final def andThen[O2](other: TransformParser[O, O2]): TransformParser[I, O2] = {
    def oldParse = parse
    new TransformParser[I, O2] {
      def parse(s: ParserInput) = {
        val thisResult  = oldParse(s)
        val otherResult = other.parse(thisResult.leftOver)
        otherResult.mapResult(or => (thisResult.result, or).mapN((f1, f2) => f1.andThen(f2)))
      }
    }
  }

  final def optional: TransformParser[I, Option[O]] = {
    def oldParse = parse
    new TransformParser[I, Option[O]] {
      def parse(s: ParserInput) = oldParse(s).mapResult {
        case Valid(f)   => Valid(f.andThen(_.some))
        case Invalid(e) => Valid(_ => None)
      }
    }
  }

  final def map[O2](f: O => O2): TransformParser[I, O2] =
    new TransformParser[I, O2] {
      def parse(s: ParserInput) = TransformParser.this.parse(s).map(_.andThen(f))
    }

  final def mapResult[O2](f: ValidatedCodec[I => O] => ValidatedCodec[I => O2]): TransformParser[I, O2] =
    new TransformParser[I, O2] {
      def parse(s: ParserInput) = TransformParser.this.parse(s).mapResult(f)
    }

  final def lift[IF, OF](mapF: (I => O) => IF => OF): TransformParser[IF, OF] =
    new TransformParser[IF, OF] {
      def parse(s: ParserInput) = TransformParser.this.parse(s).mapResult {
        case Valid(f)   => ((fi: IF) => mapF(f)(fi)).valid
        case Invalid(e) => e.invalid
      }
    }

  final def liftValidated[IF, OF](mapI: IF => I, mapO: O => ValidatedCodec[OF]): TransformParser[IF, OF] = {

    def oldParse = parse

    new TransformParser[IF, OF] {
      def parse(s: ParserInput) = {
        // TODO: try to remove try-catch hack :)
        oldParse(s).mapResult {
          case Valid(f) =>
            { (fi: IF) =>
              mapO(f(mapI(fi))) match {
                case Valid(a)   => a
                case Invalid(e) => throw InternalException(e)
              }
            }.valid

          case Invalid(e) => e.invalid
        }
      }
    }
  }

  final def lift[F[_]: Functor]: TransformParser[F[I], F[O]] =
    new TransformParser[F[I], F[O]] {
      def parse(s: ParserInput) = TransformParser.this.parse(s).mapResult {
        case Valid(f)   => ((fi: F[I]) => Functor[F].map(fi)(f)).valid
        case Invalid(e) => e.invalid
      }
    }
}

trait IsoParser[T] extends TransformParser[T, T] {
  final def repeatedWhileValid: IsoParser[T] =
    new IsoParser[T] {
      def parse(sConsume: ParserInput) = {
        @tailrec
        def helper(
            sHelper: ParserInput,
            acc: ValidatedCodec[T => T]
        ): ParserResult[T => T] =
          IsoParser.this.parse(sHelper) match {
            case ParserResult(Valid(v), l) => helper(l, acc.map(_.andThen(v)))
            case r                         => r.mapResult(_ => acc)
          }
        helper(sConsume, identity[T].valid)
      }
    }

  def toListParser(default: T) = {
    def oldParse = parse
    new IsoParser[List[T]] {
      def parse(s: ParserInput) =
        oldParse(s).map((f: T => T) => (l: List[T]) => l :+ f(default))
    }
  }
}

object IsoParser {
  def makeParallel[T](transformers: IsoParser[T]*) =
    new IsoParser[T] {
      def parse(s: ParserInput): ParserResult[T => T] =
        transformers
          .map(_.parse(s))
          .filter(_._1.isValid)
          .headOption
          .getOrElse(ParserResult.error(s"None of the parallel transformers succeeded for input '$s'", s)) // TODO: make correct location
    }

  def stringConsumer[T](ss: String) = new IsoParser[T] {
    def parse(s: ParserInput) = {
      val regex = s"$ss".r
      s.str.strip.split("\n").toList match {
        case str :: rest if regex.matches(str) =>
          ParserResult(
            identity[T].valid,
            ParserInput(
              rest.mkString("\n"),
              Location.modifyBoth(_ + 1, _ => 0)(s.location)
            )
          )
        case _ => ParserResult.error(f"Expected '$ss', but didn't find it", s)
      }
    }
  }

  //  // TODO: use
  // def singleLineRegexConsumer[T](regex: Regex, onMatch: String => T => T) = new IsoParser[T] {
  //   def parse(s: ParserInput) = {
  //     val (lines, modifyLocation) = s.str.skipBlank
  //     val newLocation             = modifyLocation(s.location)
  //     lines match {
  //       case head :: tail if regex.matches(head) =>
  //         ParserResult(
  //           onMatch(head).valid,
  //           ParserInput(
  //             tail.mkString("\n"),
  //             Location.modifyBoth(_ + 1, _ => 0)(newLocation)
  //           )
  //         )
  //       case _ => ParserResult.error(f"Unable to parse string with regex: $regex", s)
  //     }
  //   }
  // }

}

extension [G](t: TransformParser[G, G])
  def asIso: IsoParser[G] =
    new IsoParser[G] {
      def parse(s: ParserInput) = t.parse(s)
    }
