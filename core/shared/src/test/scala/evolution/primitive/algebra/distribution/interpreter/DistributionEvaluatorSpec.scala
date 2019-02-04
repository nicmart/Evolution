package evolution.primitive.algebra.distribution.interpreter

import cats.implicits._
import evolution.algebra.representation.RNGRepr
import evolution.random.RNG
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FreeSpec, Inspectors, Matchers }
import evolution.data.EvaluationModule._

import scala.util.Random

class DistributionEvaluatorSpec extends FreeSpec with Matchers with Inspectors with GeneratorDrivenPropertyChecks {
  import initial._
  "A Distribution evaluator" - {
    "should generate uniformly numbers" in {
      forAll { (a: Double, b: Double, seed: Long) =>
        val (from, to) = (Math.min(a, b), Math.max(a, b))
        val elements = materializeExpr(seed, Uniform(Dbl(a), Dbl(b))).take(5).toList
        elements should have size 5
        Inspectors.forAll(elements) { element =>
          element should (be >= from and be <= to)
        }
      }
    }
  }
}
