package dwayne.codec

import dwayne.org.ParseResult

trait Codec[A] {
  extension (s: String) def decode: ParseResult[A]
  extension (a: A) def encode: String
}
