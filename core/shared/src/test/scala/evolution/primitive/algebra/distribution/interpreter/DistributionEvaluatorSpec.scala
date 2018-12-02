package evolution.primitive.algebra.distribution.interpreter

import cats.implicits._
import evolution.algebra.representation.RNGRepr
import evolution.data.Result
import evolution.primitive.algebra.distribution.Distribution
import evolution.random.RNG
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FreeSpec, Inspectors, Matchers }

import scala.util.Random

class DistributionEvaluatorSpec extends FreeSpec with Matchers with Inspectors with GeneratorDrivenPropertyChecks {

  lazy val interpreter: Distribution[RNGRepr, Result] =
    DistributionEvaluator
  import interpreter._

  "A Distribution evaluator" - {
    "should generate uniformly numbers" in {
      forAll { (a: Double, b: Double) =>
        val (from, to) = (Math.min(a, b), Math.max(a, b))
        val elements = uniform(a.pure[Result], b.pure[Result]).sample(5)
        elements should have size 5
        Inspectors.forAll(elements) { element =>
          element should (be >= from and be <= to)
        }
      }
    }
  }

  implicit class Ops[T](rngRepr: Result[RNGRepr[T]]) {
    def sample(n: Int): List[T] =
      rngRepr.evaluate.unfold(new RNG(Random.nextLong())).take(n).toList
  }
}
