package parser

import parser.enumerations.{ExpressionKind, ExpressionType, StatementKind}
import parser.models.TreeNode
import scanner.Scannable
import scanner.enumerations.TokenType
import scanner.enumerations.TokenType.{Else, Eof, Id, If, LeftBrace, LeftParen, Num, Assign, Op_Equal, Op_LessThan, Op_Minus, Op_MoreThan, Op_Plus, Print, Read, RightBrace, RightParen, Semicolon, String, While}
import scanner.models.Token

case class Parser(scanner: Scannable) extends Parsable {
  val oneToken = 1
  val twoTokens = 2

  override def parse: Either[String, TreeNode] = {
    consumeTokenWithAction(statementSequences)
  }

  private def statementSequences(token: Token): Either[String, TreeNode] = {
    val either = statement(token)

    either.flatMap(t => {
      if (t.statementOrExpression == Left(StatementKind.Empty) || lookupOneToken.tokenType == Eof) {
        Right(t)
      } else {
        val eitherInsideBlock = consumeTokenWithAction(statementSequences)

        eitherInsideBlock match {
          case Right(st) => Right(t.copy(sibling = t.sibling :+ st))
          case l@Left(_) => l
        }
      }
    })
  }

  private def statement(token: Token): Either[String, TreeNode] = token.tokenType match {
    case If => ifStatement()
    case While => whileStatement()
    case Id => identifierStatement(token)
    case Read => readStatement()
    case Print => printStatement()
    case RightBrace | Eof => Right(TreeNode(Left(StatementKind.Empty), token))
  }

  private def ifStatement(): Either[String, TreeNode] = {
    val condition =
      matchToken(TokenType.LeftParen)
        .flatMap(_ => expression)
        .flatMap(treeNode => matchToken(TokenType.RightParen) match {
          case Right(_) => Right(treeNode)
          case Left(v) => Left(v)
        })

    val statement =
      matchToken(TokenType.LeftBrace)
        .flatMap(_ => consumeTokenWithAction(statementSequences))

    if (lookupOneToken.tokenType == Else) {
      val elseStatement =
        matchToken(TokenType.Else)
          .flatMap(_ => matchToken(TokenType.LeftBrace))
          .flatMap(_ => consumeTokenWithAction(statementSequences))

      (condition, statement, elseStatement) match {
        case (Right(c), Right(st), Right(el)) => Right(TreeNode(Array(c, st, el), Left(StatementKind.If), Token(TokenType.If)))
        case (l@Left(_), _, _) => l
        case (_, l@Left(_), _) => l
        case (_, _, l@Left(_)) => l
      }
    } else {
      (condition, statement) match {
        case (Right(c), Right(st)) => Right(TreeNode(Array(c, st), Left(StatementKind.If), Token(TokenType.If)))
        case (l@Left(_), _) => l
        case (_, l@Left(_)) => l
      }
    }
  }

  private def whileStatement(): Either[String, TreeNode] = {
    val condition =
      matchToken(TokenType.LeftParen)
        .flatMap(_ => expression)
        .flatMap(treeNode => matchToken(TokenType.RightParen) match {
          case Right(_) => Right(treeNode)
          case Left(s) => Left(s)
        })

    val statement =
      matchToken(TokenType.LeftBrace)
        .flatMap(_ => consumeTokenWithAction(statementSequences))

    (condition, statement) match {
      case (Right(c), Right(st)) => Right(TreeNode(Array(c, st), Left(StatementKind.While), Token(TokenType.While)))
      case (l@Left(_), _) => l
      case (_, l@Left(_)) => l
    }
  }

  private def identifierStatement(idToken: Token): Either[String, TreeNode] = {
    val assignNode = TreeNode(Left(StatementKind.Assign), Token(TokenType.Assign))
    val idNode = TreeNode(Right(ExpressionKind.Identifier, ExpressionType.Unknown), idToken)

    matchToken(Assign)
      .flatMap(_ => {
        val tokens = scanner.lookup(twoTokens).getOrElse(Vector())
        val tokenTypes = tokens.map(_.tokenType)

        if (tokens.size == twoTokens && tokenTypes == Vector(Id, Semicolon)) {
          matchToken(Id)
            .flatMap(id => matchToken(Semicolon) match {
              case Right(_) => Right(assignNode.copy(child = Array(idNode, idNode.copy(token = id))))
              case Left(s) => Left(s)
            })
        } else {
          expression
            .flatMap(exp => matchToken(Semicolon) match {
              case Right(_) => Right(assignNode.copy(child = Array(idNode, exp)))
              case Left(s) => Left(s)
            })
        }
      })
  }

  private def readStatement(): Either[String, TreeNode] = {
    val token = consumeOneToken.getOrElse(Token(TokenType.Error))
    val identifierNode = TreeNode(Right(ExpressionKind.Identifier, ExpressionType.Unknown), token)

    matchToken(Semicolon).flatMap(_ => {
      token.tokenType match {
        case Id => Right(TreeNode(child = Array(identifierNode), Left(StatementKind.Read), Token(TokenType.Read)))
        case _ => ???
      }
    })
  }

  private def printStatement(): Either[String, TreeNode] = {
    val printNode = TreeNode(Left(StatementKind.Print), Token(TokenType.Print))
    val twoTokensLookup = scanner.lookup(twoTokens).getOrElse(Vector())
    val tokenTypes = twoTokensLookup.map(_.tokenType)

    if (twoTokensLookup.size == twoTokens && tokenTypes == Vector(Id, Semicolon)) {
      matchToken(Id)
        .flatMap(id => matchToken(Semicolon) match {
          case Right(_) => Right(printNode.copy(child = Array(TreeNode(Right(ExpressionKind.Identifier, ExpressionType.Unknown), id))))
          case Left(s) => Left(s)
        })
    } else {
      expression
        .flatMap(exp => matchToken(Semicolon) match {
          case Right(_) => Right(printNode.copy(child = Array(exp)))
          case Left(s) => Left(s)
        })
    }
  }

  private def expression: Either[String, TreeNode] = {
    val valueLeftTreeNode = consumeTokenWithAction(expressionSimpleValues)
    val operationTreeNode = consumeTokenWithAction(expressionOperation)
    val valueRightTreeNode = consumeTokenWithAction(expressionSimpleValues)

    (valueLeftTreeNode, operationTreeNode, valueRightTreeNode) match {
      case (Right(l), Right(op), Right(r)) => Right(op.copy(child = Array(l, r)))
      case (Left(l), _, _) => Left(l)
      case (_, Left(l), _) => Left(l)
      case (_, _, Left(l)) => Left(l)
    }
  }

  private def expressionSimpleValues(token: Token): Either[String, TreeNode] = token.tokenType match {
    case Num => Right(TreeNode(Right(ExpressionKind.Constant, ExpressionType.Integer), token))
    case String => Right(TreeNode(Right(ExpressionKind.Constant, ExpressionType.String), token))
    case Id => Right(TreeNode(Right(ExpressionKind.Identifier, ExpressionType.Unknown), token))
    case LeftParen => expression
                        .flatMap(exp => matchToken(RightParen) match {
                                          case Right(_) => Right(exp)
                                          case Left(s) => Left(s)
                                })
    case _ => ??? //TODO call error handling
  }

  private def expressionOperation(token: Token): Either[String, TreeNode] = {
    val operationType = token.tokenType match {
      case Op_Plus | Op_Minus  => ExpressionType.Integer
      case Op_Equal | Op_LessThan | Op_MoreThan => ExpressionType.Boolean
      case _ => ??? //TODO call error handling
    }
    Right(TreeNode(Right(ExpressionKind.Operation, operationType), token))
  }


  private def matchToken(expectedTokenType: TokenType): Either[String, Token] = {
    val tokenOption = consumeOneToken
    if (tokenOption.isDefined && tokenOption.get.tokenType == expectedTokenType) Right(tokenOption.get)
    else Left(s"Unexpected character at position x \n Expected tokenOption $expectedTokenType" +
      s"Instead got $tokenOption") // TODO Move error message
  }

  private def consumeTokenWithAction(action: Token => Either[String, TreeNode]): Either[String, TreeNode] = {
    val tokenOption = consumeOneToken

    if (tokenOption.isDefined) {
      val token = tokenOption.get
      action(token)
    } else {
      Left("Unexpected file end.")
    }
  }

  private def consumeOneToken: Option[Token] =
    scanner.consume(oneToken).map(_.head)

  private def lookupOneToken: Token =
    scanner.lookup(oneToken).map(_.head).getOrElse(Token(TokenType.Error))
}
