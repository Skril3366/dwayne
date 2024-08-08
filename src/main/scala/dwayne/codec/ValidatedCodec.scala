package dwayne.codec

import cats.data.ValidatedNec
import cats.syntax.validated.*

type ValidatedCodec[T] = ValidatedNec[CodecError, T]

case class CodecError(
    message: String,
    location: Option[Location] = None,
    cause: Option[Throwable] = None
)

object ValidatedCodec {
  // def error[T](
  //     message: String,
  //     location: Option[Location] = None,
  //     cause: Option[Throwable] = None
  // ): ValidatedCodec[T] =
  //   CodecError(message, location, cause).invalidNec
}
