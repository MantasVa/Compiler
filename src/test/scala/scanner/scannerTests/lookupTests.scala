package scanner.scannerTests

import org.scalatest.funsuite.AnyFunSuite
import scanner.enumerations.TokenType
import scanner.models.Token
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import scanner.Scanner
import inputReader.{Readable, StringReader}
import org.scalatest.prop.TableFor3

class lookupTests extends AnyFunSuite {

  val scanCode: TableFor3[String, Int, Vector[Token]] =
    Table(
      ("SourceCode", "ScanCount", "ExpectedTokens"),
      ("      ", 100, Vector[Token](Token(TokenType.Eof))),
      ("      ", 1, Vector[Token](Token(TokenType.Eof))),
      ("  x = 5", 100, Vector(Token(TokenType.Id, "x"), Token(TokenType.Assign), Token(TokenType.Num, "5"), Token(TokenType.Eof))),
      ("  x = 5", 1, Vector(Token(TokenType.Id, "x"))),
      (" x = \"Hello world\";", 100, Vector(Token(TokenType.Id, "x"), Token(TokenType.Assign), Token(TokenType.String, "Hello world"), Token(TokenType.Semicolon), Token(TokenType.Eof))),
      (" x = \"Hello world\";", 3, Vector(Token(TokenType.Id, "x"), Token(TokenType.Assign), Token(TokenType.String, "Hello world")))
    )

  test("Correct token sequence is returned after performing token lookup") {
    forAll(scanCode) { (sourceCode, scanCount, expectedTokens) =>
      val scanner = Scanner(StringReader(sourceCode))

      //Doing it twice to make sure, tokens are not consumed
      assert(scanner.lookup(scanCount).contains(expectedTokens))
      assert(scanner.lookup(scanCount).contains(expectedTokens))
    }
  }
}
