package scanner.enumerations

sealed trait TokenType
object TokenType {
  case object If extends TokenType
  case object Else extends TokenType
  case object While extends TokenType
  case object Read extends TokenType
  case object Print extends TokenType

  case object Id extends TokenType
  case object Num extends TokenType
  case object String extends TokenType

  case object Op_Assign extends TokenType
  case object Op_Equal extends TokenType
  case object Op_LessThan extends TokenType
  case object Op_MoreThan extends TokenType
  case object Op_Plus extends TokenType
  case object Op_Minus extends TokenType

  case object LeftParen extends TokenType
  case object RightParen extends TokenType
  case object LeftBrace extends TokenType
  case object RightBrace extends TokenType
  case object Comma extends TokenType
  case object Semicolon extends TokenType

  case object Eof extends TokenType
  case object Error extends TokenType
}

