package scanner.models

import scanner.enumerations.TokenType

case class Token(tokenType: TokenType = TokenType.Error, value: String = "")
