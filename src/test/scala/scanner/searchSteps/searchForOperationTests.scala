package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.SearchSteps.searchForOperation
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForOperationTests extends AnyFunSuite{

  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),
      ("  ==5", Token(TokenType.OP_EQUAL)),
      ("= 10", Token(TokenType.OP_ASSIGN)),
      ("< 10", Token(TokenType.OP_LESSTHAN)),
      ("> 10", Token(TokenType.OP_MORETHAN)),
      ("+ 10", Token(TokenType.OP_PLUS)),
      ("   + 10", Token(TokenType.OP_PLUS)),
      ("- 10", Token(TokenType.OP_MINUS)),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForOperation(sourceCode).getOrElse(None) == expectedToken)
    }
  }
}
