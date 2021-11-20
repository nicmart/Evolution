package evolution.compiler.term

import evolution.compiler.phases.typer.config.NativeSymbolsConfig.terms

object OptimizedTermInterpreter extends TermInterpreter:
  private val baseInterpreter = RegisterBasedInterpreter()
  private val optimizer = TermOptimizer(baseInterpreter)
  override def interpret(term: Term): Any =
    baseInterpreter.interpret(optimizer.optimize(term, terms))
