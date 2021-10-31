package evolution.compiler.term

import evolution.compiler.phases.typer.config.ConstConfig.constantsTerms

object OptimizedTermInterpreter extends TermInterpreter:
  private val baseInterpreter = RegisterBasedInterpreter()
  private val optimizer = TermOptimizer(baseInterpreter)
  override def interpret(term: Term): Any =
    baseInterpreter.interpret(optimizer.optimize(term, constantsTerms))
