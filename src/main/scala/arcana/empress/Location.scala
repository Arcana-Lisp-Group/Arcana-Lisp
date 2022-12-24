package arcana.empress

case class Position(
  line: Int,
  column: Int
) {
  def :+(offset: Int) = Position(line, column + offset)
}

case class Location(
  start: Position,
  end: Position,
  filename: Option[String]
)
