package scanner.models

import scanner.enumerations.TokenType

case class Token(tokenType: TokenType, value: String = "")
