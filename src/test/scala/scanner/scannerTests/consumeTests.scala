package scanner.scannerTests

import inputReader.{Readable, StringReader}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.TableFor3
import scanner.Scanner
import scanner.enumerations.TokenType
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import scanner.models.Token

class consumeTests extends AnyFunSuite{

  val scanCode: TableFor3[String, Int, Vector[Token]] =
    Table(
      ("SourceCode", "ScanCount", "ExpectedTokens"),
      ("      ", 100, Vector[Token](Token(TokenType.EOF))),
      ("      ", 1, Vector[Token](Token(TokenType.EOF))),
      ("  x = 5", 100, Vector(Token(TokenType.ID, "x"), Token(TokenType.OP_ASSIGN), Token(TokenType.NUM, "5"), Token(TokenType.EOF))),
      ("  x = 5", 1, Vector(Token(TokenType.ID, "x"))),
      (" x = \"Hello world\";", 100, Vector(Token(TokenType.ID, "x"), Token(TokenType.OP_ASSIGN), Token(TokenType.STRING, "Hello world"), Token(TokenType.SEMICOLON), Token(TokenType.EOF))),
      (" x = \"Hello world\";", 3, Vector(Token(TokenType.ID, "x"), Token(TokenType.OP_ASSIGN), Token(TokenType.STRING, "Hello world")))
    )

  test("Correct token sequence is returned after performing token consumption") {
    forAll(scanCode) { (sourceCode, scanCount, expectedTokens) =>
      val scanner = Scanner(StringReader(sourceCode))

      //Doing it twice to make sure, tokens are consumed.
      // Not doing in case of EOF, because it will result in the same token set.
      val consumedTokens = scanner.consume(scanCount)
      assert(consumedTokens == Some(expectedTokens))

      if(consumedTokens.get.head != Token(TokenType.EOF)){
        assert(scanner.consume(scanCount) != consumedTokens)
      }
    }
  }
}
