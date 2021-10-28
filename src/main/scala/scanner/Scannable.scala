package scanner

import scanner.models.Token

trait Scannable {
  // Returns next x tokens in a source code sequence, but not moves the cursor over it.
  def lookup(count: Int): Option[Vector[Token]]

  // Returns next x tokens in a source code sequence and moves cursor over it.
  def consume(count: Int): Option[Vector[Token]]
}
