package scanner

import inputReader.Readable
import scanner.ScanSteps._
import scanner.enumerations.{ScanMode, TokenType}
import scanner.models.Token

import scala.annotation.tailrec

case class Scanner(reader: Readable) extends Scannable {

  private val tokenSeparationRegex = "( )+|[(=<>;]".r
  private val scannerSteps: Seq[String => Option[Token]] = Seq(
    source => searchForReservedWord(source),
    source => searchForOperation(source),
    source => searchForIdentifier(source),
    source => searchForString(source),
    source => searchForNumber(source),
    source => searchForSyntaxTokens(source))

  override def lookup(scanCount: Int): Option[Vector[Token]] =
    scanSourceCode(ScanMode.Lookup, scanCount, scanCount)

  override def consume(scanCount: Int): Option[Vector[Token]] =
    scanSourceCode(ScanMode.Consume, scanCount, scanCount)

  def scanSourceCode(scanMode: ScanMode, scanCount: Int, readCount: Int): Option[Vector[Token]] = {
    val sourceCodeOption = reader.lookup(readCount)

    val tokensOption =
      sourceCodeOption.flatMap(source => {
      val trimmedSource = source.trim
      val trimmedBlankSpaceCount = source.length - trimmedSource.length

      val matchIterator = tokenSeparationRegex.findAllIn(trimmedSource)
      val eofReached = source.length < readCount

      if (matchIterator.size > scanCount || eofReached) {
        val tokens = getTokens(scanCount, trimmedSource, Vector(), eofReached)

        if (scanMode == ScanMode.Consume) {
          val consumeCount = tokens.map(t => Lookup.getTokenLengthInSource(t)).sum + trimmedBlankSpaceCount
          reader.consume(consumeCount)
        }
        Some(tokens)
      }
      else {
        scanSourceCode(scanMode, scanCount, getNextReadCount(readCount))
      }
    })

    tokensOption
  }

  @tailrec
  private def getTokens(count: Int, source: String, tokens: Vector[Token], eofReached: Boolean): Vector[Token] = {
    val trimmedSource = source.trim

    if (tokens.size == count || (tokens.nonEmpty && tokens.last == Token(TokenType.Eof))) {
      tokens
    }
    else if (eofReached && trimmedSource.isEmpty) {
      getTokens(count, trimmedSource, tokens :+ Token(TokenType.Eof), eofReached)
    }
    else {
      val token = scannerSteps.foldLeft(None: Option[Token])((tokenOption, scannerStep) => {
        if (tokenOption.isDefined) {
          tokenOption
        } else {
          scannerStep(trimmedSource)
        }
      }).getOrElse(Token(TokenType.Error))

      val tokenLength = Lookup.getTokenLengthInSource(token)
      getTokens(count, trimmedSource.drop(tokenLength), tokens :+ token, eofReached)
    }
  }

  private def getNextReadCount(prevReadCount: Int): Int = prevReadCount * 2
}
