package dwayne.codec

trait Codec[A] {
  extension (s: String) def decode: ValidatedCodec[A]
  extension (a: A) def encode: String
}
