package parser

import parser.enumerations.{ExpressionKind, ExpressionType, StatementKind}
import parser.models.TreeNode
import scanner.Scannable
import scanner.enumerations.TokenType
import scanner.enumerations.TokenType.{Id, If, LeftParen, Num, Op_Equal, Op_LessThan, Op_Minus, Op_MoreThan, Op_Plus, Print, Read, While}
import scanner.models.Token

import scala.annotation.tailrec

case class Parser(scanner: Scannable) extends Parsable {
  val oneToken = 1

  override def parse: Either[String, TreeNode] = {
    consumeTokenWithAction(statementSequences)
  }

  private def statementSequences(token: Token): Either[String, TreeNode] = {
    statement(token)
  }

  private def statement(token: Token): Either[String, TreeNode] = token.tokenType match {
    case If => ifStatement()
    case While => ???
    case Id => ???
    case Read => ???
    case Print => ???
  }

  private def ifStatement(): Either[String, TreeNode] = {
    val ifTreeNode =
      matchToken(TokenType.LeftParen)
      .map(_ => expression)
      .flatMap(treeNode => {
        matchToken(TokenType.RightParen) match {
          case Right(_) => treeNode
          case Left(v) => Left(v)
        }
      })
      .map(t => TreeNode(Array(t), Left(StatementKind.If), Token(TokenType.If)))

    ifTreeNode
  }

  //  private def whileStatement(): Either[TreeNode, String] = {
  //
  //  }
  //
  //  private def identifierStatement(): Either[TreeNode, String] = {
  //
  //  }
  //
  //  private def readStatement(): Either[TreeNode, String] = {
  //
  //  }
  //
  //  private def writeStatement(): Either[TreeNode, String] = {
  //
  //  }

  private def expression: Either[String, TreeNode] = {
    val valueLeftTreeNode = consumeTokenWithAction(expressionSimpleValues)
    val operationTreeNode = consumeTokenWithAction(expressionOperation)
    val valueRightTreeNode = consumeTokenWithAction(expressionSimpleValues)

    (valueLeftTreeNode, operationTreeNode, valueRightTreeNode) match {
      case (Right(l), Right(op), Right(r)) => Right(op.copy(child = Array(l, r)))
      case (Left(l), _, _) => Left(l)
      case (_,Left(l), _) => Left(l)
      case (_, _, Left(l)) => Left(l)
    }
  }

  private def expressionSimpleValues(token: Token): Either[String, TreeNode] = token.tokenType match {
    case Num => Right(TreeNode(Right(ExpressionKind.Constant, ExpressionType.Integer), token))
    case Id => Right(TreeNode(Right(ExpressionKind.Identifier, ExpressionType.Integer), token))
    case LeftParen => expression
    case _ => ??? //TODO call error handling
  }

  private def expressionOperation(token: Token): Either[String, TreeNode] = {
    val operationType = token.tokenType match {
      case Op_Plus | Op_Minus => ExpressionType.Integer
      case Op_Equal | Op_LessThan | Op_MoreThan => ExpressionType.Boolean
      case _ => ??? //TODO call error handling
    }
    Right(TreeNode(Right(ExpressionKind.Operation, operationType), token))
  }


  private def matchToken(expectedTokenType: TokenType): Either[String, Token] = {
    val tokenOption = scanOneToken
    if (tokenOption.isDefined && tokenOption.get.tokenType == expectedTokenType) Right(tokenOption.get)
    else Left(s"Unexpected character at position x \n Expected tokenOption $expectedTokenType" +
      s"Instead got $tokenOption") // TODO Move error message
  }

  private def consumeTokenWithAction(action: Token => Either[String, TreeNode]): Either[String, TreeNode] = {
    val tokenOption = scanOneToken

    if (tokenOption.isDefined) {
      val token = tokenOption.get
      action(token)
    } else {
      Left("Unexpected file end.")
    }
  }

  private def scanOneToken: Option[Token] =
    scanner.consume(oneToken).map(_.head)
}
