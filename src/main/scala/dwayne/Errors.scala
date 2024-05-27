package dwayne

trait AppExeption extends Throwable {
  def message: String
}

case class FileNotFound(fileName: String) extends AppExeption {
  override def message: String = s"File not found: $fileName"
}

case class UnexpectedError(cause: Throwable) extends AppExeption {
  override def message: String =
    s"Unexpected error: ${Option(cause.getMessage).getOrElse("No message")}"
}
