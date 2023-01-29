package arcana.empress

sealed trait Token

case class KeywordToken(keyword: String) extends Token
case class PunctuationToken(val punc: Char) extends Token
case class IdentifierToken(name: String) extends Token
case class LiteralToken(value: String) extends Token {
  // TODO: Check types of literal values
}

// If something wrong happens in lexer,
// send the error message in the token stream.
// When parsing, ignore error tokens and emit diagnostics.
case class ErrorToken(info: String) extends Token
