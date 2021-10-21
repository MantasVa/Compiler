package scanner.lookupTests

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.Lookup.getTokenLengthInSource
import scanner.enumerations.TokenType
import scanner.models.Token

class getTokenLengthInSourceTests extends AnyFunSuite {

  val scanCode: TableFor2[Token, Int] =
    Table(
      ("Token", "Length"),
      (Token(TokenType.IF), 2),
      (Token(TokenType.ELSE), 4),
      (Token(TokenType.WHILE), 5),
      (Token(TokenType.READ), 4),
      (Token(TokenType.PRINT), 5),

      (Token(TokenType.ID, "_dentif"), 7),
      (Token(TokenType.ID, "listLength"), 10),

      (Token(TokenType.NUM, "4555"), 4),
      (Token(TokenType.NUM, "123456"), 6),

      (Token(TokenType.OP_EQUAL), 2),
      (Token(TokenType.OP_ASSIGN), 1),
      (Token(TokenType.OP_LESSTHAN), 1),
      (Token(TokenType.OP_MORETHAN), 1),
      (Token(TokenType.OP_PLUS), 1),
      (Token(TokenType.OP_PLUS), 1),
      (Token(TokenType.OP_MINUS), 1),

      (Token(TokenType.LEFTPAREN), 1),
      (Token(TokenType.LEFTBRACE), 1),
      (Token(TokenType.RIGHTBRACE), 1),
      (Token(TokenType.RIGHTPAREN), 1),
      (Token(TokenType.COMMA), 1),
      (Token(TokenType.SEMICOLON), 1),

      (Token(TokenType.EOF), 0),
      (Token(TokenType.ERROR), 0)
    )

  test("Correct length of token value is returned for appropriate token") {
    forAll(scanCode) { (token, length) =>
      assert(getTokenLengthInSource(token) == length)
    }
  }
}
