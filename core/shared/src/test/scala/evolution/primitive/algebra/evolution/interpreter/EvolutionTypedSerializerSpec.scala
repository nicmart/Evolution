package evolution.primitive.algebra.evolution.interpreter
import evolution.geometry.Point
import org.scalatest.{ FreeSpec, Matchers }
import evolution.primitive.algebra.evolution.interpreter.Types._

class EvolutionTypedSerializerSpec extends FreeSpec with Matchers {
  val interpreter = new EvolutionTypedSerializer
  import interpreter.bind._, interpreter.chain.{ empty => emptyEvolution }, interpreter.chain._, interpreter.constants._

  "EvolutionTypedSerializer" - {
    "should serialize with type information" - {
      "evolutions" - {
        "of doubles" - {
          "empty" in {
            emptyEvolution.infer(evolutionOfDoubles).toString shouldBe "empty: F[Double]"
          }

          "cons" in {
            cons(double(1), emptyEvolution)
              .infer(evolutionOfDoubles)
              .toString shouldBe "cons(1.0: Double, empty: F[Double]): F[Double]"
          }

          "mapEmpty" in {
            val actual = mapEmpty[Double](emptyEvolution, emptyEvolution).infer(evolutionOfDoubles)
            val expected = "mapEmpty(empty: F[Double], empty: F[Double]): F[Double]"
            actual.toString shouldBe expected
          }

          "mapCons" in {
            pending
            val actual =
              mapCons[Double, Double](emptyEvolution)(
                lambda[Double, F[Double] => F[Double]](
                  "head",
                  lambda[F[Double], F[Double]]("tail", emptyEvolution[Double]))
              ).infer(evolutionOfDoubles)
            val expected =
              "mapCons(empty: F[Double], head -> tail -> empty: Double -> F[Double] -> F[Double]): F[Double]"
            actual.toString shouldBe expected
          }
        }

        "of points" - {
          "empty" in {
            emptyEvolution.infer(evolutionOfPoints).toString shouldBe "empty: F[Point]"
          }

          "cons" in {
            cons(point(double(1), double(0)), emptyEvolution)
              .infer(evolutionOfPoints)
              .toString shouldBe "cons(point(1.0: Double, 0.0: Double): Point, empty: F[Point]): F[Point]"
          }

          "mapEmpty" in {
            val actual = mapEmpty[Point](emptyEvolution, emptyEvolution).infer(evolutionOfPoints)
            val expected = "mapEmpty(empty: F[Point], empty: F[Point]): F[Point]"
            actual.toString shouldBe expected
          }
        }
      }

      "constants" - {
        "an app that returns a double" in {
          val expr = app[Point, Double](lambda("p", double(2)), point(double(0), double(0)))
          val expected =
            "app(p -> 2.0: Point -> Double, point(0.0: Double, 0.0: Double): Point): Double"
          expr.infer(doubleConstant).toString shouldBe expected
        }

        // TODO this is the culprit
        "an app that returns a double taking an empty evolution as input" in {
          pending
          val expr = app[F[Double], Double](lambda("p", double(0)), emptyEvolution[Double])
          val expected =
            "app(p -> 0.0: F[Double] -> Double, 0.0: Double): Double"
          expr.infer(doubleConstant).toString shouldBe expected
        }

        "a point" in {
          point(double(0), double(1)).infer(pointConstant).toString shouldBe "point(0.0: Double, 1.0: Double): Point"
        }

        "a fix" in {
          fix(lambda[Double, Double]("x", double(0)))
            .infer(doubleConstant)
            .toString shouldBe "fix(x -> 0.0: Double -> Double): Double"
        }
      }
    }
  }
}
