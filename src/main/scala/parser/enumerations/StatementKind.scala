package parser.enumerations

sealed trait StatementKind
object StatementKind{
  case object If extends StatementKind
  case object Repeat extends StatementKind
  case object Assign extends StatementKind
  case object Read extends StatementKind
  case object Write extends StatementKind
}


