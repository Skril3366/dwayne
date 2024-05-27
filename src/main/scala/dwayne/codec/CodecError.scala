package dwayne.codec

case class CodecError(
    message: String,
    location: Option[Location],
    cause: Option[Throwable] = None
)

case class Location(
    line: Int,
    character: Option[Int] = None
)
