package scanner.searchSteps

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.{TableFor1, TableFor2}
import scanner.ScanSteps.searchForReservedWord
import scanner.enumerations.TokenType
import scanner.models.Token

class searchForReservedWordTests extends AnyFunSuite {
  val scanCode: TableFor2[String, Token] =
    Table(
      ("SourceCode", "ExpectedToken"),

      ("   if(x==5)", Token(TokenType.IF)),
      ("   if (x==5)", Token(TokenType.IF)),
      ("   if  (x==5)", Token(TokenType.IF)),
      ("if  (x==5)", Token(TokenType.IF)),

      ("   else(x==5)", Token(TokenType.ELSE)),
      ("   else (x==5)", Token(TokenType.ELSE)),
      ("else  (x==5)", Token(TokenType.ELSE)),

      ("   while(x==5)", Token(TokenType.WHILE)),
      ("   while (x==5)", Token(TokenType.WHILE)),
      ("while  (x==5)", Token(TokenType.WHILE)),

      ("   read x", Token(TokenType.READ)),
      ("   read y", Token(TokenType.READ)),
      ("read  c", Token(TokenType.READ)),

      ("   print x", Token(TokenType.PRINT)),
      ("   print y", Token(TokenType.PRINT)),
      ("print  c", Token(TokenType.PRINT)),
    )

  test("Correct token is returned with correct source code") {
    forAll(scanCode) { (sourceCode, expectedToken) =>
      assert(searchForReservedWord(sourceCode).getOrElse(None) == expectedToken)
    }
  }

  val invalidReservedKeywordCode: TableFor1[String] =
    Table(
      "SourceCode",
      "   ifg = 5",
      "   elsebg = 5",
      "   whileIden = 5",
      "   readwIden = 5",
      "   Print_Iden = 5",
    )

  test("Search returns none with source which does not contain reserved keyword") {
    forAll(invalidReservedKeywordCode) { sourceCode =>
      assert(searchForReservedWord(sourceCode).getOrElse(None) == None)
    }
  }
}
