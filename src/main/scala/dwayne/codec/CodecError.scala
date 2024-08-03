package dwayne.codec

import cats.data.ValidatedNel
import cats.syntax.validated.*

type ValidatedCodec[T] = ValidatedNel[CodecError, T]

case class CodecError(
    message: String,
    location: Option[Location] = None,
    cause: Option[Throwable] = None
)

object ValidatedCodec {
  def error[T](
      message: String,
      location: Option[Location] = None,
      cause: Option[Throwable] = None
  ): ValidatedCodec[T] =
    CodecError(message, location, cause).invalid.toValidatedNel
}

case class Location(
    line: Int,
    character: Option[Int] = None
)
