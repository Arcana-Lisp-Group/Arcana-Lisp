package arcana.emperor

import arcana.empress._

class Parser(tokens: List[(Token, Option[Location])], options: ParserOptions) {
  lazy val file = File(parse(tokens, List()))(None)

  private def parse(
    tokens: List[(Token, Option[Location])],
    previous: List[ASTNode with Expression]
  ): List[ASTNode with Expression] = tokens match {
    case (PunctuationToken('('), _) :: tail => parseList(tail) match {
      case (res, rest) => parse(rest, previous :+ res)
    }
    case (PunctuationToken('['), pos) :: tail =>
      parse(tail.dropWhile(!_.equals(PunctuationToken(']'))), previous :+ Error(s"unexpected '['.")(pos))
    case (PunctuationToken('{'), pos) :: tail =>
      parse(tail.dropWhile(!_.equals(PunctuationToken('}'))), previous :+ Error(s"unexpected '{'.")(pos))
    case (token: IdentifierToken, pos) :: tail =>
      parse(tail, previous :+ SingleExpression(Left(token))(pos))
    case (token: LiteralToken, pos) :: tail =>
      parse(tail, previous :+ SingleExpression(Right(token))(pos))
    case (ErrorToken(info), pos) :: tail => parse(tail, previous :+ Error(info)(pos))
    case (PunctuationToken(punc), pos) :: tail => parse(tail, previous :+ Error(s"unexpected $punc.")(pos))
    case (KeywordToken(kw), pos) :: tail => parse(tail, previous :+ Error(s"unexpected $kw.")(pos))
    case Nil => previous
  }

  private def parseList(
    tokens: List[(Token, Option[Location])]
  ): (ASTNode with Expression, List[(Token, Option[Location])]) = tokens match {
    case (KeywordToken("def"), pos) :: tail => parseDefinition(tail, pos)
    case (KeywordToken("fun"), pos) :: tail => parseAbstraction(tail)
    case (KeywordToken("let"), pos) :: tail => parseLetBinding(tail)
    case (KeywordToken("if"), pos) :: tail => parseCondition(tail)
    case (PunctuationToken('('), _) :: tail => parseList(tail) match {
      case (res, (PunctuationToken(')'), _) :: rest) => (res, rest)
      case (_, (token, pos) :: rest) => (Error("need ')'.")(pos), (token, pos) +: rest)
      case (_, Nil) => (Error("unexpected eof.")(None), Nil)
    }
    case (PunctuationToken('['), pos) :: tail =>
      (Error("unexpected '['.")(pos), tail.dropWhile(!_.equals(PunctuationToken(']'))))
    case (PunctuationToken('{'), pos) :: tail =>
      (Error("unexpected '{'.")(pos), tail.dropWhile(!_.equals(PunctuationToken('}'))))
    case (token: IdentifierToken, pos) :: tail => parseEvaluation(tail)
    case (token: LiteralToken, pos) :: (PunctuationToken(')'), _) :: tail =>
      (SingleExpression(Right(token))(pos), tail)
    case (token: LiteralToken, pos) :: tail =>
      (Error("need close bracket.")(pos), tail)
    case (ErrorToken(info), pos) :: tail => (Error(info)(pos), tail)
    case (PunctuationToken(punc), pos) :: tail => (Error(s"unexpected $punc.")(pos), tail)
    case (KeywordToken(kw), pos) :: tail => (Error(s"unexpected $kw.")(pos), tail)
    case Nil => (Error("unexpected eof.")(None), Nil)
  }

  private def parseDefinition(tokens: List[(Token, Option[Location])], startPos: Option[Location]) = tokens match {
    case (IdentifierToken(name), _) :: rest => {
      val (ty, expHead) = rest match {
        case (IdentifierToken(typeName), pos) :: rest if (typeName.startsWith(":")) =>
          (TypeAnnotation(typeName.substring(1))(pos), rest)
        case _ => (InferNeededType(), rest)
      }

      val (exp, tail) = expHead match {
        case (PunctuationToken('('), _) :: tail => parseList(tail)
        case (token: IdentifierToken, pos) :: tail => (SingleExpression(Left(token))(pos), tail)
        case (token: LiteralToken, pos) :: tail => (SingleExpression(Right(token))(pos), tail)
        case (ErrorToken(info), pos) :: tail => (Error(info)(pos), tail)
        case (_, pos) :: tail => (Error("expect expression to be binded.")(pos), tail)
        case Nil => (Error("unexpected eof.")(None), Nil)
      }

      val fullPos = (startPos, tail) match {
        case (Some(startPos), (_, Some(endPos)) :: _) => Some(Location(startPos.start, endPos.end, startPos.filename))
        case _ => None
      }
      (Definition(name, ty, exp)(fullPos), tail)
    }
    case (_, pos) :: tail => (Error("need variable name after 'def'.")(pos), tail)
    case Nil => (Error("unexpected eof.")(None), Nil)
  }

  // TODO:
  private def parseAbstraction(tokens: List[(Token, Option[Location])]) = ???
  private def parseLetBinding(tokens: List[(Token, Option[Location])]) = ???
  private def parseCondition(tokens: List[(Token, Option[Location])]) = ???
  private def parseEvaluation(tokens: List[(Token, Option[Location])]) = ???
}
