package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.ScanSteps.searchForIdentifier
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForIdentifierTests extends AnyFunSuite {

  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),

      (" _identif ==5", Token(TokenType.ID, "_identif")),
      (" myNumber2 = 10", Token(TokenType.ID, "myNumber2")),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForIdentifier(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
