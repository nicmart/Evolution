package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.types.TypeClassInstance.NumericInst
import Term._
import Literal._
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Predicate

class TermOptimizerTest extends LanguageSpec {
  "TermOptimizer should optimize" - {
    "application of literals" - {
      "integers" in {
        val term = App(Lit(LitInt(2)), Inst(numDouble))

        optimize(term) shouldBe Value(2)
      }
    }
  }

  lazy val optimizer = new TermOptimizer(new TermInterpreter)

  def optimize(term: Term): Term = optimizer.optimize(term)

  lazy val numDouble = TypingConfig.instance(Predicate("Num", List(Type.Double))).unsafeRight
}
