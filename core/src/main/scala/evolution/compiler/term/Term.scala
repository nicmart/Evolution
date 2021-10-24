package evolution.compiler.term

import evolution.compiler.term.Term.Literal
import evolution.compiler.types.TypeClassInstance

enum Term:
  case Lit(l: Literal)
  case Id(name: String)
  case Inst(instance: TypeClassInstance)
  case Let(name: String, expr: Term, body: Term)
  case Lambda(name: String, body: Term)
  case Apply(f: Term, x: Term)
  case Value(value: Any)

object Term:
  enum Literal:
    case LitInt(n: Int)
    case LitBool(b: Boolean)
    case LitDouble(d: Double)
    case LitList(ts: List[Term])
