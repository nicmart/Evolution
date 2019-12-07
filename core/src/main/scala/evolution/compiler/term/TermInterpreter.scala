package evolution.compiler.term

import Term._
import Literal._

// F will be String for JsMaterialization, Register => Any for EvalMaterialization
class TermInterpreter {
  def interpret(term: Term): Any = RegisterBasedInterpreter.fresh.interpret(term)
}

class RegisterBasedInterpreter(register: Map[String, Any]) {
  def interpret(term: Term): Any = term match {
    case Lit(LitInt(n))        => n
    case Lit(LitBool(b))       => b
    case Lit(LitDouble(d))     => d
    case Lit(LitList(terms))   => ???
    case Id(name)              => ???
    case Let(name, expr, body) => ???
    case Lambda(name, body)    => ???
    case App(f, x)             => ???
    case PLambda(pName, body)  => ???
    case PApp(term, arg)       => ???
  }
}

object RegisterBasedInterpreter {
  def fresh: RegisterBasedInterpreter = new RegisterBasedInterpreter(Map.empty)
}
