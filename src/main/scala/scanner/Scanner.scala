package scanner

import scanner.SearchSteps._
import scanner.enumerations.TokenType
import scanner.models.Token

object Scanner {

  private val scannerSteps: Seq[String => Option[Token]] = Seq(source => searchForReservedWord(source),
                                                               source => searchForOperation(source),
                                                               source => searchForIdentifier(source),
                                                               source => searchForString(source),
                                                               source => searchForNumber(source),
                                                               source => searchForSyntaxTokens(source))
  //TODO WRITE INPUT READER LIKE A MODULE, SO YOU CAN CHANGE IT AND LET LEXER USE INPUT AS STRING, IO, Whatever
  def scanSource(sourceCode: String): Option[Token] = {
    if (sourceCode.isBlank) {
      Some(Token(TokenType.EOF))
    } else {
      scannerSteps.foldLeft(None: Option[Token])((tokenOption, scannerStep) => {
        if(tokenOption.isEmpty){
          scannerStep(sourceCode)
        }else{
          tokenOption
        }
      })
    }
  }

}
