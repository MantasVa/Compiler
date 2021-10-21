package scanner

import scanner.enumerations.TokenType
import scanner.enumerations.TokenType.{COMMA, ELSE, EOF, ERROR, ID, IF, LEFTBRACE, LEFTPAREN, NUM, OP_ASSIGN, OP_EQUAL, OP_LESSTHAN, OP_MINUS, OP_MORETHAN, OP_PLUS, PRINT, READ, RIGHTBRACE, RIGHTPAREN, SEMICOLON, STRING, WHILE}
import scanner.models.Token

import scala.util.matching.Regex

object Lookup {

  private val oneChar = 1
  private val twoChar = 2

  val reservedWordsLookup: Map[String, Token] = Map(
    ("if", Token(TokenType.IF)),
    ("else", Token(TokenType.ELSE)),
    ("while", Token(TokenType.WHILE)),
    ("read", Token(TokenType.READ)),
    ("print", Token(TokenType.PRINT))
  )

  val operationsLookup: Map[String, Token] = Map(
    ("==", Token(TokenType.OP_EQUAL)),
    ("=", Token(TokenType.OP_ASSIGN)),
    ("<", Token(TokenType.OP_LESSTHAN)),
    (">", Token(TokenType.OP_MORETHAN)),
    ("+", Token(TokenType.OP_PLUS)),
    ("-", Token(TokenType.OP_MINUS)),
  )

  val syntaxLookup: Map[String, Token] = Map(
    ("(", Token(TokenType.LEFTPAREN)),
    (")", Token(TokenType.RIGHTPAREN)),
    ("{", Token(TokenType.LEFTBRACE)),
    ("}", Token(TokenType.RIGHTBRACE)),
    (",", Token(TokenType.COMMA)),
    (";", Token(TokenType.SEMICOLON)),
  )

  def getTokenLengthInSource(token: Token): Int = token match {
    case Token(ID, value) => value.length
    case Token(NUM, value) => value.length
    case Token(STRING, value) => value.length + twoChar // adding two " " double quotes that are emitted during scanning
    case Token(tokenType, _) if tokenType == IF | tokenType == ELSE |
                                tokenType == WHILE | tokenType == READ |
                                tokenType == PRINT => tokenType.toString.length
    case Token(OP_EQUAL, _) => twoChar
    case Token(OP_ASSIGN, _) | Token(OP_LESSTHAN, _) |
         Token(OP_MORETHAN, _) | Token(OP_PLUS, _)  |
         Token(OP_MINUS, _) => oneChar
    case Token(LEFTPAREN, _) | Token(RIGHTPAREN, _) |
         Token(LEFTBRACE, _) | Token(RIGHTBRACE, _) |
         Token(COMMA, _) | Token( SEMICOLON, _) => oneChar
    case Token(EOF, _) => 0
    case Token(ERROR, value) => value.length
  }

  def findRegexByToken(tokenType: TokenType): Regex = tokenType match {
    case ID => "[_a-zA-Z][_a-zA-z0-9]*".r
    case NUM => "[0-9]+".r
    case STRING => "\"[^\"\\n]*\"".r
    case OP_EQUAL => "==".r
    case OP_ASSIGN => "=[^=]".r
    case OP_LESSTHAN => "<".r
    case OP_MORETHAN => ">".r
    case OP_PLUS => "\\+".r
    case OP_MINUS => "-".r
    case LEFTPAREN => "\\(".r
    case RIGHTPAREN => "\\)".r
    case LEFTBRACE => "\\{".r
    case RIGHTBRACE => "}".r
    case COMMA => ",".r
    case SEMICOLON => ";".r
  }
}
