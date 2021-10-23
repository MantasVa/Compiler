package parser

import parser.enumerations.{ExpressionKind, ExpressionType, StatementKind}
import scanner.models.Token

case class TreeNode(child: Array[TreeNode],
                    sibling: Array[TreeNode],
                    statExpr: Either[StatementKind, (ExpressionKind, ExpressionType)],
                    lineNumber: Int,
                    token: Token
                   )
