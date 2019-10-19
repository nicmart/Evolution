package evolution.compiler.impl.evaluation

import evolution.compiler.LanguageSpec
import evolution.materialization.Evolution
import evolution.materialization.Evolution._
import evolution.geometry.Point
import org.scalatest.Inspectors
import evolution.compiler.expression.typeclass._
import evolution.compiler.impl.evaluation.MaterializeAddition
import evolution.compiler.impl.evaluation.MaterializeInverse

class EvolutionSpec extends LanguageSpec {
  "Evolutions" - {
    "derivatives" - {
      "derivative of a constant is 0" in {
        val deriving = constant[Double](1)
        val derivative =
          derive(deriving, MaterializeAddition(Additive.DoubleDoubleDouble), MaterializeInverse(Invertible.Double))
        derivative.run.take(10).toList shouldBe List.fill(10)(0)
      }

      "derivative of a constant evolution of points the zero point" in {
        val deriving = constant[Point](Point(100, 100))
        val derivative =
          derive(
            deriving,
            MaterializeAddition(Additive.PointPointPoint),
            MaterializeInverse(Invertible.Point)
          )
        derivative.run.take(10).toList shouldBe List.fill(10)(Point.zero)
      }

      "derivative gives the differences" in {
        val deriving = cons[Double](1.0, cons(2, cons(3, cons(4, cons(5, Evolution.empty)))))
        val derivative =
          derive(deriving, MaterializeAddition(Additive.DoubleDoubleDouble), MaterializeInverse(Invertible.Double))
        derivative.run.toList shouldBe List.fill(4)(1.0)
      }
    }

    "uniformFrom" - {
      "returns an empty evolution if n <= 0" in {
        uniformFrom(0, Evolution.empty).run.toList should be(empty)
        uniformFrom(1, Evolution.empty).run.toList should be(empty)
      }

      "returns an empty evolution if sampling evolution is empty" in {
        uniformFrom(1, Evolution.empty).run.toList should be(empty)
      }
    }

    "uniformDiscrete" - {
      "should be empty when positive step and start > stop" in {
        uniformDiscrete(100, 0, 1).run.take(10).toList should be(empty)
      }

      "should be empty when negative step and start < stop" in {
        uniformDiscrete(0, 100, -1).run.take(10).toList should be(empty)
      }

      "should be const(start) when step = 0" in {
        uniformDiscrete(2, 100, 0).run.take(100).toList should be(List.fill(100)(2))
      }

      "should gives results between start and from and where result-from is a multiple of step" in {
        val result = uniformDiscrete(1, 100, 3).run.take(10).toList
        result should have size (10)
        all(result) shouldBe >=(1.0)
        all(result) shouldBe <=(100.0)
        Inspectors.forAll(result) { n =>
          (n - 1) % 3 shouldBe 0
        }
      }
    }

    "uniformChoice" - {
      "should generate elements in the list" in {
        val ns = uniformChoice(List.range(1, 1000)).run.take(1000).toList
        Inspectors.forAll(ns) { n =>
          n should be >= (1)
          n should be <= (1000)
        }
      }

      "should generate all elements" in {
        val list = List.range(1, 5)
        val ns = uniformChoice(list).run.take(10000).toList
        Inspectors.forAll(list) { n =>
          ns should contain(n)
        }
      }
    }

    "range" - {
      "should be empty when positive step and start > stop" in {
        range(100, 0, 1).run.take(10).toList should be(empty)
      }

      "should be empty when negative step and start < stop" in {
        range(0, 100, -1).run.take(10).toList should be(empty)
      }

      "should be const(start) when step = 0" in {
        range(2, 100, 0).run.take(100).toList should be(List.fill(100)(2))
      }

      "should gives all results from from to start" in {
        val result = range(1, 100, 3).run.take(100).toList
        result shouldBe (1 to 100 by 3).toList
      }
    }
  }
}
