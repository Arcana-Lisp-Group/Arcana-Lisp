package arcana.empress

// The starting position of a file is (1, 0)
case class Position(
  line: Int,
  column: Int
) {
  def :+(offset: Int) = Position(line, column + offset)
}

// Locations indicate positions [start, end) in files.
case class Location(
  start: Position,
  end: Position,
  filename: Option[String]
)
