package parser.ParserTests

import inputReader.StringReader
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor1
import parser.Parser
import scanner.Scanner

class parseTests extends AnyFunSuite {

  val parseCode: TableFor1[String] =
    Table(
      "sourceCode",
      "if (5 < 10)",
    )

  test("Correct parse tree is returned") {
    forAll(parseCode) { sourceCode =>
      val reader = StringReader(sourceCode)
      val scanner = Scanner(reader)
      val parser = Parser(scanner)


      val either = parser.parse
      assert(either.isRight)
    }
  }

}
