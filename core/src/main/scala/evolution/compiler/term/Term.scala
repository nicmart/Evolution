package evolution.compiler.term

import evolution.compiler.types.TypeClassInstance

sealed trait Term

object Term {
  case class Lit(l: Literal) extends Term
  case class Id(name: String) extends Term
  case class Inst(instance: TypeClassInstance) extends Term
  case class Let(name: String, expr: Term, body: Term) extends Term
  case class Lambda(name: String, body: Term) extends Term
  case class Apply(f: Term, x: Term) extends Term
  case class Value(value: Any) extends Term

  sealed abstract class Literal
  object Literal {
    case class LitInt(n: Int) extends Literal
    case class LitBool(b: Boolean) extends Literal
    case class LitDouble(d: Double) extends Literal
    case class LitList(ts: List[Term]) extends Literal
  }
}
