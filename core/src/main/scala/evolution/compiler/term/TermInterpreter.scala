package evolution.compiler.term

import scala.collection.mutable
import Term._
import Literal._

// F will be String for JsMaterialization, Register => Any for EvalMaterialization
class TermInterpreter {
  def interpret(term: Term): Any = RegisterBasedInterpreter.fresh.interpret(term)
}

class RegisterBasedInterpreter private (register: mutable.Map[String, Any]) {
  def interpret(term: Term): Any = term match {
    case Lit(LitInt(n))        => n
    case Lit(LitBool(b))       => b
    case Lit(LitDouble(d))     => d
    case Lit(LitList(terms))   => ???
    case Id(name)              => register(name)
    case Let(name, expr, body) => ???
    case Lambda(name, body)    => ???
    case App(f, x)             => ???
    case PLambda(pName, body)  => ???
    case PApp(term, arg)       => ???
  }

  def bind(name: String, value: Any): Unit = register.update(name, value)
}

object RegisterBasedInterpreter {
  def fresh: RegisterBasedInterpreter = new RegisterBasedInterpreter(mutable.Map.empty)
}
