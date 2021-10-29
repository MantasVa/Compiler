package scanner.scannerTests

import inputReader.{Readable, StringReader}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.TableFor3
import scanner.Scanner
import scanner.enumerations.TokenType
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import scanner.models.Token

import scala.annotation.tailrec

class consumeTests extends AnyFunSuite{

  val scanCode: TableFor3[String, Int, Vector[Token]] =
    Table(
      ("SourceCode", "ScanCount", "ExpectedTokens"),
      ("      ", 100, Vector[Token](Token(TokenType.Eof))),
      ("      ", 1, Vector[Token](Token(TokenType.Eof))),
      ("  x = 5", 100, Vector(Token(TokenType.Id, "x"), Token(TokenType.Op_Assign), Token(TokenType.Num, "5"), Token(TokenType.Eof))),
      ("  x = 5", 1, Vector(Token(TokenType.Id, "x"))),
      (" x = \"Hello world\";", 100, Vector(Token(TokenType.Id, "x"), Token(TokenType.Op_Assign), Token(TokenType.String, "Hello world"), Token(TokenType.Semicolon), Token(TokenType.Eof))),
      (" x = \"Hello world\";", 3, Vector(Token(TokenType.Id, "x"), Token(TokenType.Op_Assign), Token(TokenType.String, "Hello world"))),
      ("if (5 < 10)", 7, Vector(Token(TokenType.If), Token(TokenType.LeftParen), Token(TokenType.Num, "5"), Token(TokenType.Op_LessThan), Token(TokenType.Num, "10"), Token(TokenType.RightParen), Token(TokenType.Eof)))
    )

  test("Correct token sequence is returned after performing token consumption") {
    forAll(scanCode) { (sourceCode, scanCount, expectedTokens) =>
      val scanner = Scanner(StringReader(sourceCode))

      //Doing it twice to make sure, tokens are consumed.
      // Not doing in case of EOF, because it will result in the same token set.
      val consumedTokens = scanner.consume(scanCount)
      assert(consumedTokens.contains(expectedTokens))

      if(consumedTokens.get.head != Token(TokenType.Eof)){
        assert(scanner.consume(scanCount) != consumedTokens)
      }
    }
  }

  test("Tokens consumed correctly when consuming one at a time") {
    val sourceCode = "if (5 < 10)"
    val scanner = Scanner(StringReader(sourceCode))

    val expectedVector = Vector(Token(TokenType.If), Token(TokenType.LeftParen), Token(TokenType.Num, "5"), Token(TokenType.Op_LessThan), Token(TokenType.Num, "10"), Token(TokenType.RightParen), Token(TokenType.Eof))

    @tailrec
    def go(vector: Vector[Token]): Vector[Token] = {
      if(vector.nonEmpty && vector.last.tokenType == TokenType.Eof) vector
      else go(vector :+ scanner.consume(1).map(_.head).get)
    }

    val actual = go(Vector())
    assert(actual == expectedVector)
  }
}
