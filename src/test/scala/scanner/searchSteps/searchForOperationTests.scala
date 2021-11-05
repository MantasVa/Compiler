package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.ScanSteps.searchForOperation
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForOperationTests extends AnyFunSuite{

  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),
      ("  ==5", Token(TokenType.Op_Equal)),
      ("= 10", Token(TokenType.Assign)),
      ("< 10", Token(TokenType.Op_LessThan)),
      ("> 10", Token(TokenType.Op_MoreThan)),
      ("+ 10", Token(TokenType.Op_Plus)),
      ("   + 10", Token(TokenType.Op_Plus)),
      ("- 10", Token(TokenType.Op_Minus)),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForOperation(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
