package parser.models

import parser.enumerations.{ExpressionKind, ExpressionType, StatementKind}
import scanner.enumerations.TokenType.{Else, Id, If, Num, Assign, Op_Equal, Op_LessThan, Op_Minus, Op_MoreThan, Op_Plus, Print, Read, RightBrace, String, While}
import scanner.models.Token

sealed case class TreeNode(child: Array[TreeNode],
                           sibling: Array[TreeNode],
                           statementOrExpression: Either[StatementKind, (ExpressionKind, ExpressionType)],
                           token: Token)

object TreeNode {

  def apply(statementOrExpression: Either[StatementKind, (ExpressionKind, ExpressionType)], token: Token): TreeNode =
    apply(Array(), statementOrExpression, token)

  def apply(child: Array[TreeNode], statementOrExpression: Either[StatementKind, (ExpressionKind, ExpressionType)], token: Token): TreeNode =
    apply(child, Array(), statementOrExpression, token)

  def apply(child: Array[TreeNode], sibling: Array[TreeNode], statementOrExpression: Either[StatementKind, (ExpressionKind, ExpressionType)], token: Token): TreeNode = {
    val treeNode = new TreeNode(child, sibling, statementOrExpression, token)

    if(isTreeNodeValid(treeNode)) treeNode else throw new IllegalArgumentException
  }

  def isTreeNodeValid(treeNode: TreeNode): Boolean = {
    if (treeNode.statementOrExpression.isLeft) {
      treeNode.token.tokenType match {
        case If | Read | Print | Else | While | RightBrace | Assign => true
        case _ => false
      }
    }
    else {
      treeNode.token.tokenType match {
        case Op_Equal | Op_Plus | Op_Minus | Op_MoreThan | Op_LessThan | Num | Id | String => true
        case _ => false
      }
    }
  }
}
