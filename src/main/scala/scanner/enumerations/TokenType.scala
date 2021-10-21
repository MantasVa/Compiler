package scanner.enumerations

sealed trait TokenType
object TokenType {
  case object IF extends TokenType
  case object ELSE extends TokenType
  case object WHILE extends TokenType
  case object READ extends TokenType
  case object PRINT extends TokenType

  case object ID extends TokenType
  case object NUM extends TokenType
  case object STRING extends TokenType

  case object OP_ASSIGN extends TokenType
  case object OP_EQUAL extends TokenType
  case object OP_LESSTHAN extends TokenType
  case object OP_MORETHAN extends TokenType
  case object OP_PLUS extends TokenType
  case object OP_MINUS extends TokenType

  case object LEFTPAREN extends TokenType
  case object RIGHTPAREN extends TokenType
  case object LEFTBRACE extends TokenType
  case object RIGHTBRACE extends TokenType
  case object COMMA extends TokenType
  case object SEMICOLON extends TokenType

  case object EOF extends TokenType
  case object ERROR extends TokenType
}

