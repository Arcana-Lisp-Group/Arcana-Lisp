package arcana.emperor

import arcana.empress.{Token, Location}
import arcana.emperor._

class Parser(tokens: List[(Token, Option[Location])], options: ParserOptions) {
  lazy val file = File(parse(tokens))(None)

  private def parse(tokens: List[(Token, Option[Location])]): List[ASTNode with Expression] = ???
}
