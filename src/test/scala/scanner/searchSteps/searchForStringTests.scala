package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.ScanSteps.searchForString
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForStringTests extends AnyFunSuite{
  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),
      (" \"Hello world\"", Token(TokenType.STRING, "Hello world")),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForString(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
