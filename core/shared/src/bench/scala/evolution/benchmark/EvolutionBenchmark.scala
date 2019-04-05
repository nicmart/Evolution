package evolution.benchmark

import evolution.language.{ FullModule, InstancesModule, InterpreterModule }
import evolution.materialization.{ RNG, RNGRepr }
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionBenchmark extends FreeSpec with Matchers {
  // TODO Rubbish, rubbish, rubbish!!!
  val module = new FullModule[RNGRepr] with InstancesModule[RNGRepr] with InterpreterModule
  import evolution.data.EvaluationContext._
  import module._

  "an evolution" - {
    "should be fast" in {
      unsafeRun("@(1)", 10000) shouldBe ()
    }
  }

  private def unsafeRun(expr: String, n: Int): Unit =
    Interpreter
      .interpret[RNGRepr[Double]](
        parse("@(1)", Type.Evo(Type.Dbl), VarContext.empty)
          .asInstanceOf[TypeInferenceResult[Expr[RNGRepr[Double]]]]
          .unsafeEvaluate)
      .apply(emptyCtx)
      .iterator(RNG(0L))
      .drop(n)
}
