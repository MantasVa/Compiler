package parser.enumerations

sealed trait NodeKind
object NodeKind{
  case object Statement extends NodeKind
  case object Expression extends NodeKind
}
