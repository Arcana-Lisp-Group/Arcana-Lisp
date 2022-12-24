package arcana.empress

import arcana.empress._

object TokenPrettyPrinter {
  def apply(tokens: List[(Token, Option[Location])]): String =
    tokens.foldLeft("res:\n")((res, p) => p match {
      case (token, Some(loc)) =>
        s"$res${printToken(token)} at [(${loc.start.line}, ${loc.start.column}) (${loc.end.line}, ${loc.end.column})]"
      case (token, None) =>
        s"$res${printToken(token)}\n"
    })

  private def printToken(token: Token) = token match {
    case KeywordToken(keyword) => s"  KEYWORD($keyword)"
    case PunctuationToken(punc) => s"  PUNCTUATION($punc)"
    case IdentifierToken(name) => s"  IDENTIFIER($name)"
    case LiteralToken(value) => s"  LITERAL($value)"
    case ErrorToken(info) => s"  ERROR($info)"
  }
}
