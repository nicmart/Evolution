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
      Benchmark(
        Type.Dbl,
        "@(1)",
        Goal.MaxRngReprAllocations(0)
      ),
      Benchmark(
        Type.Dbl,
        "map(@(1), x -> x)",
        Goal.MaxRngReprAllocations(20),
        Goal.MaxExprAllocations(0)
      ),
      Benchmark(
        Type.Dbl,
        "y = 1 in map(map(@(1), x -> x), x -> y)",
        Goal.MaxRngReprAllocations(40),
        Goal.MaxExprAllocations(0)
      ),
      Benchmark(
        Type.Dbl,
        "f = x -> x in @(f(1))",
        Goal.MaxRngReprAllocations(0)
      ),
      Benchmark(
        Type.Point,
        veryLongExpression,
        Goal.MaxExprAllocations(0),
        Goal.MaxRngReprAllocations(440)
      )
    )

    benchmarks foreach { benchmark =>
      benchmark.expression.take(100).toString - {
        benchmark.run
      }
    }
  }

  case class Benchmark(tpe: Type, expression: String, goals: Goal*) {
    def run: Unit = {
      unsafeRun(tpe, expression, 10)
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

  private def unsafeRun(tpe: Type, expr: String, n: Int): Unit = {
    val iterator = Interpreter
      .interpret[RNGRepr[tpe.Out]](
        parse(expr, Type.Evo(tpe), VarContext.empty)
          .asInstanceOf[TypeInferenceResult[Expr[RNGRepr[tpe.Out]]]]
          .unsafeEvaluate
      )
      .apply(emptyCtx)
      .iterator(RNG(0L))

    resetCounts()
    RNGRepr.resetAllocationsCount()
    resetExprAllocationsCount()

    iterator.drop(n)
  }

  lazy val veryLongExpression = """
    left = -10 in right = 10 in top = 10 in bottom = -10 in
    
    grid = gridSize -> flatMap(
      range(bottom, top, gridSize),
      y -> map(
        range(left, right, gridSize),
        x -> point(x, y)
      )
    ) in
    
    onPoints = points -> drawings -> length ->
      flatten(zipWith(
        points,
        drawings,
        p -> drawing -> take(length, map(drawing, q -> q + p))
      ))
    in
    
    circle = alpha -> r -> w ->
      @polar(@(r), integrate(alpha, @(w)))
    in
    
    w1s = uniformDiscrete(-3, 3, 1) in
    w2s = uniformDiscrete(-30, 30, 2) in
    w3s = uniformDiscrete(-30, 200, 10) in
    w4s = uniformDiscrete(-200, 30, 21) in
    w5s = uniformDiscrete(-30, 30, 5) in
    
    r1s = uniformDiscrete(10, 50, 1) in
    r2s = uniformDiscrete(10, 25, 1) in
    r3s = uniformDiscrete(5, 10, 1) in
    r4s = uniformDiscrete(2, 5, 1) in
    r5s = uniformDiscrete(1, 2, 1) in
    
    
    circles = k ->
    
      zipWith(
        uniform(0, 2 * pi),
        w1s,
        w2s,
        w3s,
        w4s,
        w5s,
        r1s,
        r2s,
        r3s,
        r4s,
        r5s,
        alpha -> w1 -> w2 -> w3 -> w4 -> w5 -> r1 -> r2 -> r3 -> r4 -> r5 ->
          circle(alpha, r1, k * w1) @+
          circle(alpha, r2, k * w2) @+
          circle(alpha, r3, k * w3) @+
          circle(alpha, r4, k * w4) @+
          circle(alpha, r4, k * w5)
      )
    
    in
    
    
    k = .01 in
    length = 20 in
    
    onPoints(grid(150), circles(k), floor(length / k))
  """
}
