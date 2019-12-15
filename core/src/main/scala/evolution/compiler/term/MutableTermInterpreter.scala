package evolution.compiler.term

import evolution.compiler.impl.evaluation.MaterializeNumeric
import evolution.compiler.phases.typer.config.ConstConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term.{Apply, Id, Inst, Lambda, Let, Lit, Value}
import evolution.compiler.types.TypeClassInstance.NumericInst

import scala.collection.mutable

final class MutableTermInterpreter extends TermInterpreter {
  override def interpret(term: Term): Any = MutableTermInterpreterImpl.fresh.interpret(term)
}

private[term] final class MutableTermInterpreterImpl private (register: mutable.Map[String, Any])
    extends TermInterpreter {

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
    case Apply(f, x) =>
      interpret(f).asInstanceOf[Any => Any](interpret(x))

    case Id(name) => register(name)

    case Inst(inst) => inst

    case Value(value) => value
  }

  def bind(name: String, value: Any): Unit = register.update(name, value)
}

private[term] object MutableTermInterpreterImpl {
  def fresh: TermInterpreter =
    new MutableTermInterpreterImpl(mutable.Map.from(constants))

  val constants: Map[String, Any] = ConstConfig.constants.map(c => c.name -> c.value).toMap
}
