package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.SearchSteps.searchForSyntaxTokens
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForSyntaxTokens extends AnyFunSuite {

  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),
      ("(x = 10", Token(TokenType.LEFTPAREN)),
      ("){", Token(TokenType.RIGHTPAREN)),
      ("{x=5", Token(TokenType.LEFTBRACE)),
      ("} else {", Token(TokenType.RIGHTBRACE)),
      (", ", Token(TokenType.COMMA)),
      (";x ==5", Token(TokenType.SEMICOLON)),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForSyntaxTokens(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
