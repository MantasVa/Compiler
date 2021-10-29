package scanner

import scanner.Lookup.{findRegexByToken, operationsLookup, reservedWordsLookup, syntaxLookup}
import scanner.enumerations.TokenType
import scanner.models.Token

object ScanSteps {

  def searchForReservedWord(sourceCode: String): Option[Token] = {
    val trimmedSource = sourceCode.trim
    reservedWordsLookup.foldLeft(None: Option[Token])((tokenOption, mapTuple) => {
      if (tokenOption.isEmpty) {
        val (key, token) = mapTuple
        val spaceOrBraceRegex = "[ (]".r
        (key + spaceOrBraceRegex).r.findPrefixOf(trimmedSource).map(_ => token)
      } else {
        tokenOption
      }
    })
  }

  def searchForOperation(sourceCode: String): Option[Token] = {
    val trimmedSource = sourceCode.trim
    operationsLookup.foldLeft(None: Option[Token])((tokenOption, mapTuple) => {
      if (tokenOption.isEmpty) {
        val (_, token) = mapTuple
        findRegexByToken(token.tokenType).findPrefixOf(trimmedSource).map(_ => token)
      } else {
        tokenOption
      }
    })
  }

  def searchForIdentifier(sourceCode: String): Option[Token] = {
    val trimmedSource = sourceCode.trim
    val idTokenType = TokenType.Id
    val identifierOption = findRegexByToken(idTokenType).findPrefixOf(trimmedSource)
    identifierOption.map(value => Token(idTokenType, value))
  }

  def searchForString(sourceCode: String): Option[Token] = {
    val trimmedSource = sourceCode.trim
    val stringTokenType = TokenType.String
    val stringOption = findRegexByToken(stringTokenType).findPrefixOf(trimmedSource)
    stringOption.map(value => Token(stringTokenType, value.replace("\"", "")))
  }

  def searchForNumber(sourceCode: String): Option[Token] = {
    val trimmedSource = sourceCode.trim
    val numTokenType = TokenType.Num
    val identifierOption = findRegexByToken(numTokenType).findPrefixOf(trimmedSource)
    identifierOption.map(value => Token(numTokenType, value))
  }

  def searchForSyntaxTokens(sourceCode: String): Option[Token] = {
    val trimmedSource = sourceCode.trim
    syntaxLookup.foldLeft(None: Option[Token])((tokenOption, mapTuple) => {
      if (tokenOption.isEmpty) {
        val (_, token) = mapTuple
        findRegexByToken(token.tokenType).findPrefixOf(trimmedSource).map(_ => token)
      } else {
        tokenOption
      }
    })
  }
}