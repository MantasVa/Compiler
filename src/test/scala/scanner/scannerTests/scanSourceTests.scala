package scanner.scannerTests

import org.scalatest.funsuite.AnyFunSuite
import scanner.enumerations.TokenType
import scanner.models.Token
import scanner.Scanner.scanSource
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor2
import scanner.Lookup

class scanSourceTests extends AnyFunSuite {

  val scanCode: TableFor2[String, Vector[Token]] =
    Table(
      ("SourceCode", "ExpectedTokens"),
      ("      ", Vector[Token](Token(TokenType.EOF))),
      ("  x = 5", Vector(Token(TokenType.ID, "x"), Token(TokenType.OP_ASSIGN), Token(TokenType.NUM, "5"), Token(TokenType.EOF))),
      (" x = \"Hello world\";", Vector(Token(TokenType.ID, "x"), Token(TokenType.OP_ASSIGN), Token(TokenType.STRING, "Hello world"), Token(TokenType.SEMICOLON), Token(TokenType.EOF)))
    )

  test("Correct token sequence is returned after scanning source") {
    forAll(scanCode) { (sourceCode, expectedTokens) =>

      def recurseSource(sourceCode: String, tokens: Vector[Token]): Vector[Token] = {
        scanSource(sourceCode).map(t => {
          if (t.tokenType != TokenType.EOF) {
            val tokenLength = Lookup.getTokenLengthInSource(t)
            recurseSource(sourceCode.trim.drop(tokenLength), tokens :+ t)
          } else {
            tokens :+ t
          }
        }).getOrElse(Vector())
      }

      assert(recurseSource(sourceCode, Vector()) == expectedTokens)
    }
  }
}
