package parser.enumerations

sealed trait ExpressionType
object ExpressionType{
  case object Void extends ExpressionType
  case object Integer extends ExpressionType
  case object Boolean extends ExpressionType
  case object String extends ExpressionType
}
