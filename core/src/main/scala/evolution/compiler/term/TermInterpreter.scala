package evolution.compiler.term

import evolution.compiler.impl.evaluation.{MaterializeAddition, MaterializeNumeric}
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term._
import evolution.compiler.types.TypeClassInstance.{AdditiveInst, NumericInst}

import scala.collection.mutable

class TermInterpreter {
  def interpret(term: Term): Any = RegisterBasedInterpreter.fresh.interpret(term)
}

class RegisterBasedInterpreter private (register: mutable.Map[String, Any]) {
  def interpret(term: Term): Any = term match {
    case Lit(LitInt(n))      => (num: NumericInst[Any]) => MaterializeNumeric(num.num)(n)
    case Lit(LitBool(b))     => b
    case Lit(LitDouble(d))   => d
    case Lit(LitList(terms)) => terms.map(interpret)

    case Let(name, expr, body) =>
      bind(name, interpret(expr))
      interpret(body)

    case Lambda(name, body) =>
      (x: Any) => {
        bind(name, x)
        interpret(body)
      }
    case App(f, x) =>
      interpret(f).asInstanceOf[Any => Any](interpret(x))

    case Id(name) => register(name)

    case Inst(inst) => inst
  }

  def bind(name: String, value: Any): Unit = register.update(name, value)
}

object RegisterBasedInterpreter {
  def fresh: RegisterBasedInterpreter =
    new RegisterBasedInterpreter(mutable.Map.from(constants))

  val constants: Map[String, Any] = ConstConfig.constants.map(c => c.name -> c.value).toMap
}
