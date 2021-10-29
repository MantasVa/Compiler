package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.ScanSteps.searchForSyntaxTokens
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForSyntaxTokens extends AnyFunSuite {

  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),
      ("(x = 10", Token(TokenType.LeftParen)),
      ("){", Token(TokenType.RightParen)),
      ("{x=5", Token(TokenType.LeftBrace)),
      ("} else {", Token(TokenType.RightBrace)),
      (", ", Token(TokenType.Comma)),
      (";x ==5", Token(TokenType.Semicolon)),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForSyntaxTokens(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
