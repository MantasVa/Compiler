package parser.enumerations

sealed trait ExpressionKind
object ExpressionKind{
  case object Operation extends ExpressionKind
  case object Constant extends ExpressionKind
  case object Identifier extends ExpressionKind
}
