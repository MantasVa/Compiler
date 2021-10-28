package inputReader

case class StringReader(private var sourceCode: String) extends Readable {

  private val emptyString = ""

  override def lookup(count: Int): Option[String] =
    if (sourceCode.length >= count) Some(sourceCode.take(count)) else Some(sourceCode)

  override def consume(count: Int): Option[String] = {
    if (sourceCode.length >= count) {
      val consumedSource = sourceCode.take(count)
      sourceCode = sourceCode.splitAt(count)._2

      Some(consumedSource)
    } else {
      val consumedSource = sourceCode
      sourceCode = emptyString

      Some(consumedSource)
    }
  }
}
