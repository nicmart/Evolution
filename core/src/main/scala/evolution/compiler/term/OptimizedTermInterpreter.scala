package evolution.compiler.term

object OptimizedTermInterpreter extends TermInterpreter {
  private val baseInterpreter = new RegisterBasedInterpreter
  private val optimizer = new TermOptimizer(baseInterpreter)
  override def interpret(term: Term): Any =
    baseInterpreter.interpret(optimizer.optimize(term))
}
