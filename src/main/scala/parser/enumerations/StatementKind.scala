package parser.enumerations

sealed trait StatementKind
object StatementKind{
  case object If extends StatementKind
  case object While extends StatementKind
  case object Assign extends StatementKind
  case object Read extends StatementKind
  case object Print extends StatementKind
  case object Empty extends StatementKind
}


