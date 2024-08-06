package dwayne.codec

import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
import cats.Functor
// import monocle.Lens
import scala.annotation.tailrec

trait StringTransformer[I, O] {
  def consume(s: String): (ValidatedCodec[I => O], String)

  // final def collectWhileValid(s: String): (List[ValidatedCodec[I => O]], String) = {
  //   @tailrec
  //   def helper(
  //       ss: String,
  //       acc: List[ValidatedCodec[I => O]]
  //   ): (List[ValidatedCodec[I => O]], String) =
  //     consume(ss) match {
  //       case (Valid(f), l)   => helper(l, acc :+ f.valid)
  //       case (Invalid(_), _) => (acc, ss)
  //     }
  //   helper(s, List())
  // }

  final def error(
      leftOverStr: String,
      message: String,
      location: Option[Location] = None
  ): (ValidatedCodec[I => O], String) =
    (ValidatedCodec.error(message, location), leftOverStr)

  final def andThen[O2](other: StringTransformer[O, O2]): StringTransformer[I, O2] =
    new StringTransformer[I, O2] {
      def consume(s: String) = {
        val (v1, leftOver1) = StringTransformer.this.consume(s)
        val (v2, leftOver2) = other.consume(leftOver1)
        (v1.andThen(f1 => v2.map(f2 => f1.andThen(f2))), leftOver2)
      }
    }

  final def optional: StringTransformer[I, Option[O]] = {
    def oldConsume = this.consume
    new StringTransformer[I, Option[O]] {
      def consume(s: String) = oldConsume(s) match {
        case (Valid(f), l)   => (((x: I) => f(x).some).valid, l)
        case (Invalid(e), l) => (((_: I) => None).valid, l)
      }
    }
  }

  final def liftI[I2](lense: I2 => I): StringTransformer[I2, O] = {
    def oldConsume = this.consume
    new StringTransformer[I2, O] {
      def consume(s: String): (ValidatedCodec[I2 => O], String) = oldConsume(s) match {
        case (Valid(f), l)   => (((i2: I2) => f(lense(i2))).valid, l)
        case (Invalid(e), l) => (e.invalid, l)
      }
    }
  }

  final def liftO[O2](lense: O => I => O2): StringTransformer[I, O2] = {
    def oldConsume = this.consume
    new StringTransformer[I, O2] {
      def consume(s: String): (ValidatedCodec[I => O2], String) = oldConsume(s) match {
        case (Valid(f), l)   => (((i: I) => lense(f(i))(i)).valid, l)
        case (Invalid(e), l) => (e.invalid, l)
      }
    }
  }

  final def lift[I2, O2](iLense: I2 => I, oLense: O => I2 => O2): StringTransformer[I2, O2] =
    this.liftI[I2](iLense).liftO[O2](oLense)

  final def liftC[F[_]: Functor]: StringTransformer[F[I], F[O]] = {
    def oldConsume = this.consume
    new StringTransformer[F[I], F[O]] {
      def consume(s: String): (ValidatedCodec[F[I] => F[O]], String) = oldConsume(s) match {
        case (Valid(f), l)   => (((fi: F[I]) => Functor[F].map(fi)(f)).valid, l)
        case (Invalid(e), l) => (e.invalid, l)
      }
    }
  }
}

trait IsoStringTransformer[T] extends StringTransformer[T, T] {
  final def repeatedWhileValid: StringTransformer[T, T] = {
    def oldConsume = this.consume
    new StringTransformer[T, T] {
      def consume(sConsume: String) = {
        @tailrec
        def helper(
            sHelper: String,
            acc: ValidatedCodec[T => T]
        ): (ValidatedCodec[T => T], String) =
          oldConsume(sHelper) match {
            case (Valid(v), l) => helper(l, acc.map(_.andThen(v)))
            case _             => (acc, sHelper)
          }
        helper(sConsume, identity[T].valid)
      }
    }
  }

  def toListTransformer = {
    def oldConsume = consume
    new IsoStringTransformer[List[T => T]] {
      def consume(s: String): (ValidatedCodec[List[T => T] => List[T => T]], String) = {
        val (v: ValidatedCodec[T => T], rest) = oldConsume(s)
        (v.map((f: T => T) => (l: List[T => T]) => l :+ f), rest)
      }
    }
  }
}

object IsoStringTransformer {
  def makeParallel[T](transformers: IsoStringTransformer[T]*) =
    new IsoStringTransformer[T] {
      def consume(s: String): (ValidatedCodec[T => T], String) =
        transformers
          .map(_.consume(s))
          .filter(_._1.isValid)
          .headOption
          .getOrElse(error(s, s"None of the parallel transformers succeeded for input '$s'"))
    }

  def stringConsumer[T](ss: String) = new IsoStringTransformer[T] {
    def consume(s: String) = {
      val regex = s"$ss".r
      s.strip.split("\n").toList match {
        case regex() :: rest => (identity[T].valid, rest.mkString("\n"))
        case _               => error(s, f"Expected '$ss', but didn't find it in '${s.strip}'")
      }
    }
  }
}

extension [G](t: StringTransformer[G, G])
  def asIso: IsoStringTransformer[G] =
    new IsoStringTransformer[G] {
      def consume(s: String) = t.consume(s)
    }
