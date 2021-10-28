package inputReader

trait Readable {
  // Reads next x chars in a source code sequence, but not moves the cursor over it.
  def lookup(count: Int): Option[String]

  // Reads next x chars in a source code sequence and moves cursor over it.
  def consume(count: Int): Option[String]
}
