package evolution.benchmark

import evolution.language.{ FullModule, InstancesModule, InterpreterModule }
import evolution.materialization.{ RNG, RNGRepr }
import org.scalatest.{ FreeSpec, Matchers }

import scala.concurrent.duration.Duration

class EvolutionBenchmark extends FreeSpec with Matchers {
  // TODO Rubbish, rubbish, rubbish!!!
  val module = new FullModule[RNGRepr] with InstancesModule[RNGRepr] with InterpreterModule
  import evolution.data.EvaluationContext._
  import module._

  "an evolution" - {
    "should be fast" in {
      val result = benchmark(10)(unsafeRun("@(1)", 60000))
      result.average.toMillis should be < 10L
    }
  }

  case class BenchmarkResult(times: Int, totalDuration: Duration) {
    def average: Duration = totalDuration / times
  }

  private def benchmark[T](times: Int)(chunk: => T): BenchmarkResult = {
    var totalDuration = 0L
    (1 to times).foreach { _ =>
      val start = System.nanoTime()
      chunk
      val duration = System.nanoTime() - start
      totalDuration += duration
    }

    BenchmarkResult(times, Duration.fromNanos(totalDuration))
  }

  private def unsafeRun(expr: String, n: Int): Unit =
    Interpreter
      .interpret[RNGRepr[Double]](
        parse(expr, Type.Evo(Type.Dbl), VarContext.empty)
          .asInstanceOf[TypeInferenceResult[Expr[RNGRepr[Double]]]]
          .unsafeEvaluate)
      .apply(emptyCtx)
      .iterator(RNG(0L))
      .drop(n)
}
