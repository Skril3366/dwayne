package dwayne.codec

case class Location(
    line: Int,
    column: Int
)

object Location {
  def nextLine(l: Location): Location                    = Location(l.line + 1, 0)
  def modifyLine(f: Int => Int)(l: Location): Location   = l.copy(line = f(l.line))
  def modifyColumn(f: Int => Int)(l: Location): Location = l.copy(column = f(l.column))
  def modifyBoth(
      line: Int => Int,
      column: Int => Int
  )(l: Location): Location = Location(
    line(l.line),
    column(l.column)
  )
}
