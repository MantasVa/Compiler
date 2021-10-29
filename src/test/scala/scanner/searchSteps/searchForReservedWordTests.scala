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

      ("   if(x==5)", Token(TokenType.If)),
      ("   if (x==5)", Token(TokenType.If)),
      ("   if  (x==5)", Token(TokenType.If)),
      ("if  (x==5)", Token(TokenType.If)),

      ("   else(x==5)", Token(TokenType.Else)),
      ("   else (x==5)", Token(TokenType.Else)),
      ("else  (x==5)", Token(TokenType.Else)),

      ("   while(x==5)", Token(TokenType.While)),
      ("   while (x==5)", Token(TokenType.While)),
      ("while  (x==5)", Token(TokenType.While)),

      ("   read x", Token(TokenType.Read)),
      ("   read y", Token(TokenType.Read)),
      ("read  c", Token(TokenType.Read)),

      ("   print x", Token(TokenType.Print)),
      ("   print y", Token(TokenType.Print)),
      ("print  c", Token(TokenType.Print)),
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
