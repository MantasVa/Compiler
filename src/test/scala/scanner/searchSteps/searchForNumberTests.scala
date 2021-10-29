package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.ScanSteps.searchForNumber
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForNumberTests extends AnyFunSuite {
  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),
      ("  4114", Token(TokenType.Num, "4114")),
      ("1234+22", Token(TokenType.Num, "1234")),
      ("759< 10", Token(TokenType.Num, "759")),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForNumber(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
