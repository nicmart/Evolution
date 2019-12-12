package evolution.compiler.term

import Term._
import evolution.compiler.term.Term.Literal.LitInt

final class TermOptimizer(interpreter: TermInterpreter) {
  def optimize(term: Term): Term = term match {

    case App(Lit(LitInt(_)), Inst(_)) => Value(interpreter.interpret(term))

    case _ => term
  }
}
