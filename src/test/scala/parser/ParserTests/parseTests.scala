package parser.ParserTests

import helpers.Printer
import inputReader.StringReader
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.prop.TableDrivenPropertyChecks.{Table, forAll}
import org.scalatest.prop.TableFor1
import parser.Parser
import parser.enumerations.StatementKind
import parser.models.TreeNode
import scanner.Scanner
import scanner.enumerations.TokenType
import scanner.models.Token

class parseTests extends AnyFunSuite {

  val parseCode: TableFor1[String] =
    Table(
      "sourceCode",
      "if (5 < 10) { }",
      "if (5 < 10) { if(7<8) {} }",
      "if (5 < 10) { if(7<8) { if(7<8) { }} }",
      "if (5 < 10) { } else { }",
      "if (5 < 10) { } else { if(7<8) {} }",
      "if (5 > 10) { }",
      "if (5 == 10) { }",
      "if (x == 10) {} ",
      "if (\"hello world\" == \"Hello World\") {} ",
      "while(5 < 0) {  }",
      "while(5 < 0) { if (5 == 10) { } else { } }",
      "id = 5 - 5;",
      "id = NewVar;",
      "id = 5 + 5;",
      "myInt = 5 - 0;",
      "print 5 + 5;",
      "print myvariable;",
      "read myVar;",
      "print (5+5) + 5;",
      "myInt = (5 - (10 - 5 )) + (10 - 5);"
    )

  test("Tree node is returned after parsing") {
    forAll(parseCode) { sourceCode =>
      val reader = StringReader(sourceCode)
      val scanner = Scanner(reader)
      val parser = Parser(scanner)

      val either = parser.parse

      Printer.printBinary(either.getOrElse(TreeNode(Left(StatementKind.Assign), Token(TokenType.Error))))
      assert(either.isRight)
    }
  }


  test("Source is parsed correctly") {
    val sourceCode = "count = 1 - 0;"//\nn = 1;\nlimit = 100;\nwhile (n < limit) {\n    k=3;\n    p=1;\n    n=n+2;\n    while ( (k+k)<n) {\n        p=n-k;\n        k=k+2;\n    }\n    if (p == 5) {\n        print  \" is prime\\n\" + n;\n   count = count + 1;\n    }\n}\nprint \"Total primes found: \" + count;"
    val reader = StringReader(sourceCode)
    val scanner = Scanner(reader)
    val parser = Parser(scanner)

    val either = parser.parse

    assert(either.isRight)
  }
}
