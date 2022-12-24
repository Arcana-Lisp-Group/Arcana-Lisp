package arcana.empress

import arcana.empress._

class Lexer(code: String, options: LexerOptions) {
  private type LocatedToken = (Token, Option[Location])
  lazy val tokens: List[LocatedToken] = parse()
  
  private def parse(): List[LocatedToken] =
    code.split("\n").iterator.zipWithIndex.map(
      {case (line, i) => parseLine(List(), 0)(line, i + 1)}).toList.flatten

  import Lexer.{keywords, punctuations}
  private def parseLine(parsed: List[LocatedToken], column: Int = 0)(implicit str: String, line: Int): List[LocatedToken] =
    if (column == str.length()) parsed
    else {
      if (str(column) == ';') parsed // comments
      else if (punctuations.contains(str(column)))
        parseLine(parsed :+ parsePunctuation(str(column), Position(line, column)), column + 1)
      else if (str(column) == '"') parseStringLiteral(str, Position(line, column)) match {
        case (locatedToken, next) => parseLine(parsed :+ locatedToken, next)
      }
      else if (str(column).isDigit) parseNumericLiteral(str, Position(line, column)) match {
        case (locatedToken, next) => parseLine(parsed :+ locatedToken, next)
      }
      else parseIdentifierAndKeyword(str, Position(line, column)) match {
        case (locatedToken, next) => parseLine(parsed :+ locatedToken, next)
      }
    }

  private def createLocation(start: Position, end: Position) =
    if (options.enableLocationInfo) Some(Location(start, end, options.filename))
    else None

  private def parsePunctuation(punc: Char, position: Position) = 
    (PunctuationToken(punc), createLocation(position, position))

  private def parseStringLiteral(str: String, start: Position) = {
    val end = str.indexOf('"', start.column + 1)
    if (end == -1)
      ((ErrorToken("missing \" in the string literal"),
        createLocation(start, Position(start.line, str.length() - 1))), str.length())
    else
      ((LiteralToken(str.substring(start.column, end + 1)), createLocation(start, Position(start.line, end))), end + 1)
  }

  private def parseNumericLiteral(str: String, start: Position, allowDotOrE: Boolean = true): (LocatedToken, Int) = {
    val num = str.substring(start.column).takeWhile((c) => c.isDigit)
    val end = start :+ num.length()
    val next = end.column + 1

    if (str(next) == '.' || str(next) == 'e' || str(next) == 'E') {
      parseNumericLiteral(str, Position(start.line, next + 1), false) match {
        case ((ErrorToken(info), Some(loc)), next) => ((ErrorToken(info), createLocation(start, loc.end)), next)
        case ((ErrorToken(info), None), next) => ((ErrorToken(info), None), next)
        case ((LiteralToken(num2), Some(loc)), next) => ((LiteralToken(num + num2), createLocation(start, loc.end)), next)
        case ((LiteralToken(num2), None), next) => ((LiteralToken(num + num2), None), next)
        case _ => throw new AssertionError("fatal error during parsing numeric literal.") // this will not happen
      }
    }
    else if (punctuations.contains(str(next)) && str(next) != '(' && str(next) != '[' && str(next) != '{')
      ((LiteralToken(num), createLocation(start, end)), next + 1)
    else // e.g. 123abc
      ((ErrorToken("wrong numeric expression"), createLocation(start, end)), next + 1)
  }

  private def parseIdentifierAndKeyword(str: String, start: Position) = {
    val res = str.substring(start.column).takeWhile((c) => 
      !punctuations.contains(c))
    val end = start :+ res.length()

    if (keywords.contains(res)) ((KeywordToken(res), createLocation(start, end)), end.column + 1)
    else ((IdentifierToken(res), createLocation(start, end)), end.column + 1)
  }
}

object Lexer {
  def apply(code: String, options: LexerOptions) =
    new Lexer(code, options)

  private val keywords = List[String](
    "def", "defn", "let", "if", "match" // TODO: Add more if need
  )

  private val punctuations = List[Char](
    '(', ')', ' ', '\t', '\n', '\r', ':', '[', ']', '{', '}' // TODO: Are these symbols enough?
  )
}
