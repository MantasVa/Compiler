package scanner

import scanner.enumerations.TokenType
import scanner.enumerations.TokenType.{Comma, Else, Eof, Error, Id, If, LeftBrace, LeftParen, Num, Assign, Op_Equal, Op_LessThan, Op_Minus, Op_MoreThan, Op_Plus, Print, Read, RightBrace, RightParen, Semicolon, String, While}
import scanner.models.Token

import scala.util.matching.Regex

object Lookup {

  private val oneChar = 1
  private val twoChar = 2

  val reservedWordsLookup: Map[String, Token] = Map(
    ("if", Token(TokenType.If)),
    ("else", Token(TokenType.Else)),
    ("while", Token(TokenType.While)),
    ("read", Token(TokenType.Read)),
    ("print", Token(TokenType.Print))
  )

  val operationsLookup: Map[String, Token] = Map(
    ("==", Token(TokenType.Op_Equal)),
    ("=", Token(TokenType.Assign)),
    ("<", Token(TokenType.Op_LessThan)),
    (">", Token(TokenType.Op_MoreThan)),
    ("+", Token(TokenType.Op_Plus)),
    ("-", Token(TokenType.Op_Minus)),
  )

  val syntaxLookup: Map[String, Token] = Map(
    ("(", Token(TokenType.LeftParen)),
    (")", Token(TokenType.RightParen)),
    ("{", Token(TokenType.LeftBrace)),
    ("}", Token(TokenType.RightBrace)),
    (",", Token(TokenType.Comma)),
    (";", Token(TokenType.Semicolon)),
  )

  def getTokenLengthInSource(token: Token): Int = token match {
    case Token(Id, value) => value.length
    case Token(Num, value) => value.length
    case Token(String, value) => value.length + twoChar // adding two " " double quotes that are emitted during scanning
    case Token(tokenType, _) if tokenType == If | tokenType == Else |
                                tokenType == While | tokenType == Read |
                                tokenType == Print => tokenType.toString.length
    case Token(Op_Equal, _) => twoChar
    case Token(Assign, _) | Token(Op_LessThan, _) |
         Token(Op_MoreThan, _) | Token(Op_Plus, _) |
         Token(Op_Minus, _) => oneChar
    case Token(LeftParen, _) | Token(RightParen, _) |
         Token(LeftBrace, _) | Token(RightBrace, _) |
         Token(Comma, _) | Token( Semicolon, _) => oneChar
    case Token(Eof, _) => 0
    case Token(Error, value) => value.length
  }

  def findRegexByToken(tokenType: TokenType): Regex = tokenType match {
    case Id => "[_a-zA-Z][_a-zA-z0-9]*".r
    case Num => "[0-9]+".r
    case String => "\"[^\"\\n]*\"".r
    case Op_Equal => "==".r
    case Assign => "=[^=]".r
    case Op_LessThan => "<".r
    case Op_MoreThan => ">".r
    case Op_Plus => "\\+".r
    case Op_Minus => "-".r
    case LeftParen => "\\(".r
    case RightParen => "\\)".r
    case LeftBrace => "\\{".r
    case RightBrace => "}".r
    case Comma => ",".r
    case Semicolon => ";".r
  }
}
