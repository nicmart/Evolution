package evolution.benchmark

import evolution.language.{ FullModule, InstancesModule, InterpreterModule }
import evolution.materialization.{ RNG, RNGRepr }
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionBenchmark extends FreeSpec with Matchers {
  // TODO Rubbish, rubbish, rubbish!!!
  val module = new FullModule[RNGRepr] with InstancesModule[RNGRepr] with InterpreterModule
  import evolution.data.EvaluationContext._
  import module._

  "benchmark for evolution" - {
    val benchmarks = List(
      Benchmark("@(1)", Goal.MaxRngReprAllocations(0)),
      Benchmark("map(@(1), x -> x)", Goal.MaxRngReprAllocations(20), Goal.MaxExprAllocations(0)),
      Benchmark("y = 1 in map(map(@(1), x -> x), x -> y)", Goal.MaxRngReprAllocations(40), Goal.MaxExprAllocations(0))
      )

    benchmarks foreach { benchmark =>
      benchmark.expression - {
        benchmark.run
      }
    }
  }

  case class Benchmark(expression: String, goals: Goal*) {
    def run: Unit = {
      unsafeRun(expression, 10)
      val result = BenchmarkResult(RNGRepr.allocationsCount, interpreterRuns, outAllocations, exprAllocationsCount)
      goals.foreach { goal =>
        goal.toString in {
          assertGoal(result)(goal)
        }
      }
    }
  }

  sealed trait Goal
  object Goal {
    case class MaxRngReprAllocations(value: Int) extends Goal {
      override def toString: String = s"RngRepr allocations should be less than $value"
    }
    case class MaxExprAllocations(value: Int) extends Goal {
      override def toString: String = s"Expr allocations should be less than $value"
    }
  }

  case class BenchmarkResult(rngReprAllocations: Int, interpreterRuns: Int, outAllocations: Int, exprAllocations: Int)

  def assertGoal(result: BenchmarkResult)(goal: Goal) = goal match {
    case Goal.MaxRngReprAllocations(goal) =>
      assert(result.rngReprAllocations <= goal)
    case Goal.MaxExprAllocations(goal) =>
      assert(result.exprAllocations <= goal)
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
    resetExprAllocationsCount()

    iterator.drop(n)
  }
}
