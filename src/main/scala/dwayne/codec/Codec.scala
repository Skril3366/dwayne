package dwayne.codec

import cats.data.Validated

trait Codec[A] {
  extension (s: String) def decode: Validated[A, CodecError]
  extension (a: A) def encode: String
}
