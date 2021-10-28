package scanner.enumerations

sealed trait ScanMode
object ScanMode{
  case object Consume extends ScanMode
  case object Lookup extends ScanMode
}
