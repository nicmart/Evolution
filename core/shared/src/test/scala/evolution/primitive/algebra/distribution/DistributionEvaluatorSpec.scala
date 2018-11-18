package evolution.primitive.algebra.distribution

import cats.implicits._
import evolution.algebra.representation.RNGRepr
import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.distribution.interpreter.DistributionEvaluator
import evolution.random.RNG
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FreeSpec, Inspectors, Matchers }

import scala.util.Random

class DistributionEvaluatorSpec
    extends FreeSpec
    with Matchers
    with Inspectors
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  lazy val interpreter: Distribution[RNGRepr, EvaluationResult] =
    DistributionEvaluator
  import interpreter._

  "A Distribution evaluator" - {
    "should generate uniformly numbers" in {
      forAll { (a: Double, b: Double) =>
        val (from, to) = (Math.min(a, b), Math.max(a, b))
        val elements = uniform(a.pure[EvaluationResult], b.pure[EvaluationResult]).sample(5)
        elements should have size 5
        Inspectors.forAll(elements) { element =>
          element should (be >= from and be <= to)
        }
      }
    }
  }

  implicit class Ops[T](rngRepr: EvaluationResult[RNGRepr[T]]) {
    def sample(n: Int): List[T] =
      rngRepr.get(Nil).unfold(new RNG(Random.nextLong())).take(n).toList
  }
}
