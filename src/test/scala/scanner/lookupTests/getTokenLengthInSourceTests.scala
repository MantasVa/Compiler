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
      (Token(TokenType.If), 2),
      (Token(TokenType.Else), 4),
      (Token(TokenType.While), 5),
      (Token(TokenType.Read), 4),
      (Token(TokenType.Print), 5),

      (Token(TokenType.Id, "_dentif"), 7),
      (Token(TokenType.Id, "listLength"), 10),

      (Token(TokenType.Num, "4555"), 4),
      (Token(TokenType.Num, "123456"), 6),

      (Token(TokenType.Op_Equal), 2),
      (Token(TokenType.Assign), 1),
      (Token(TokenType.Op_LessThan), 1),
      (Token(TokenType.Op_MoreThan), 1),
      (Token(TokenType.Op_Plus), 1),
      (Token(TokenType.Op_Plus), 1),
      (Token(TokenType.Op_Minus), 1),

      (Token(TokenType.LeftParen), 1),
      (Token(TokenType.LeftBrace), 1),
      (Token(TokenType.RightBrace), 1),
      (Token(TokenType.RightParen), 1),
      (Token(TokenType.Comma), 1),
      (Token(TokenType.Semicolon), 1),

      (Token(TokenType.Eof), 0),
      (Token(TokenType.Error), 0)
    )

  test("Correct length of token value is returned for appropriate token") {
    forAll(scanCode) { (token, length) =>
      assert(getTokenLengthInSource(token) == length)
    }
  }
}
