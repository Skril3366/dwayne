package dwayne.codec

trait Encoder[A] {
  def encode(a: A): String
}

extension [A: Encoder](a: A) {
  def encode = summon[Encoder[A]].encode(a)
}
