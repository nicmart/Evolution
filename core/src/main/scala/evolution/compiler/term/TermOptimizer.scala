package evolution.compiler.term

import evolution.compiler.term.Term.Literal.LitList
import evolution.compiler.term.Term._

import scala.util.Try

final class TermOptimizer(interpreter: TermInterpreter) {
  def optimize(term: Term): Term = {
    val optimized = term match {
      case Lit(LitList(ts)) => Lit(LitList(ts.map(optimize)))
      case Lit(_) | Inst(_) => Value(interpreter.interpret(term))
      case App(f, x) =>
        (optimize(f), optimize(x)) match {
          case (Value(f), Value(x)) => Value(f.asInstanceOf[Any => Any](x))
          case (f, x)               => App(f, x)
        }
      case Let(name, body, in) => Let(name, optimize(body), optimize(in))
      case Id(_)               => Try(interpreter.interpret(term)).toOption.fold(term)(Value.apply)
      case Lambda(name, body)  => Lambda(name, optimize(body))
      case _                   => term
    }

    optimized
  }
}
