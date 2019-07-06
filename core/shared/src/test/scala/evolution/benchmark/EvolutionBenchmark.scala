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

  "benchmark for evolution" - {
    val expressions = List(
      "@(1)",
      "map(@(1), x -> x)",
      "y = 1 in map(map(@(1), x -> x), x -> y)"
      )

    expressions foreach { expression =>
      expression in {
        unsafeRun(expression, 60000)
        interpreterRuns should be(0)
        outAllocations should be(0)
        RNGRepr.allocationsCount should be(0)
      }
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

  private def unsafeRun(expr: String, n: Int): Unit = {
    val iterator = Interpreter
      .interpret[RNGRepr[Double]](
        parse(expr, Type.Evo(Type.Dbl), VarContext.empty)
          .asInstanceOf[TypeInferenceResult[Expr[RNGRepr[Double]]]]
          .unsafeEvaluate
      )
      .apply(emptyCtx)
      .iterator(RNG(0L))

    resetCounts()
    RNGRepr.resetAllocationsCount()

    iterator.drop(n)
  }
}
