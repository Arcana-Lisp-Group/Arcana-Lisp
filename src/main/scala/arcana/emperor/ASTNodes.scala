

package arcana.emperor
import arcana.empress.{Location, IdentifierToken, LiteralToken}

abstract class ASTNode()(implicit location: Option[Location])

trait Expression
trait TypeExpression

final case class File(sequence: List[ASTNode with Expression])(implicit location: Option[Location]) extends ASTNode
final case class Definition(
  name: String,
  ty: ASTNode with TypeExpression,
  value: ASTNode with Expression
)(implicit location: Option[Location]) extends ASTNode with Expression
final case class TypeAnnotation(typeName: String)(implicit location: Option[Location]) extends ASTNode with TypeExpression
final case class InferNeededType() extends ASTNode()(None) with TypeExpression
final case class Evaluation(
  sequence: List[ASTNode with Expression]
)(implicit location: Option[Location]) extends ASTNode with Expression
final case class Abstraction(
  params: List[(String, ASTNode with TypeExpression)],
  body: Evaluation
)(implicit location: Option[Location]) extends ASTNode with Expression
final case class FunctionType(
  param: ASTNode with TypeExpression,
  result: ASTNode with TypeExpression
) (implicit location: Option[Location]) extends ASTNode with TypeExpression
final case class Condition(
  test: ASTNode with Expression,
  consequent: ASTNode with Expression,
  alternate: ASTNode with Expression
)(implicit location: Option[Location]) extends ASTNode with Expression
final case class SingleExpression(
  value: Either[IdentifierToken, LiteralToken]
)(implicit location: Option[Location]) extends ASTNode with Expression
final case class Error(
  info: String
)(implicit location: Option[Location]) extends ASTNode with Expression with TypeExpression
