package dwayne.parser

case class Location(
    line: Int,
    column: Int
){
  def nextLine: Location                    = Location(line + 1, 0)
  def modifyLine(f: Int => Int): Location   = copy(line = f(line))
  def modifyColumn(f: Int => Int): Location = copy(column = f(column))
  def modifyBoth(
      line: Int => Int,
      column: Int => Int
  ): Location = Location(
    line(this.line),
    column(this.column)
  )
}

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
