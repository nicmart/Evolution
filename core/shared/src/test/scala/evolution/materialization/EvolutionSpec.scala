package evolution.materialization

import evolution.language.LanguageSpec
import evolution.typeclass.Semigroupoid
import evolution.typeclass.Invertible
import Evolution._
import evolution.geometry.Point

class EvolutionSpec extends LanguageSpec {
  "Evolutions" - {
    "derivatives" - {
      "derivative of a constant is 0" in {
        val deriving = constant[Double](1)
        val derivative = derive(deriving, Semigroupoid.Additive.dblDblDbl, Invertible.Additive.dblInvertible)
        derivative.run.take(10).toList shouldBe List.fill(10)(0)
      }

      "derivative of a constant evolution of points the zero point" in {
        val deriving = constant[Point](Point(100, 100))
        val derivative = derive(deriving, Semigroupoid.Additive.pointPointPoint, Invertible.Additive.pointInvertible)
        derivative.run.take(10).toList shouldBe List.fill(10)(Point.zero)
      }

      "derivative gives the differences" in {
        val deriving = cons[Double](1.0, cons(2, cons(3, cons(4, cons(5, Evolution.empty)))))
        val derivative = derive(deriving, Semigroupoid.Additive.dblDblDbl, Invertible.Additive.dblInvertible)
        derivative.run.toList shouldBe List.fill(4)(1.0)
      }
    }
  }
}
