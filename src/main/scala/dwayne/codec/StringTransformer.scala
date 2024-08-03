package dwayne.codec

import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
// import monocle.Lens
import scala.annotation.tailrec

trait StringTransformer[I, O] {
  opaque type ValidatedConsumptions = ValidatedCodec[I => O]

  def consume(s: String): (ValidatedCodec[I => O], String)

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
}

extension [G](t: StringTransformer[G, G])
  def asIso: IsoStringTransformer[G] =
    new IsoStringTransformer[G] {
      def consume(s: String) = t.consume(s)
    }
