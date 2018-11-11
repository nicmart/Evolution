package evolution.primitive.algebra.evolution.interpreter
import evolution.geometry.Point
import org.scalatest.{ FreeSpec, Matchers }
import evolution.primitive.algebra.evolution.interpreter.Types._

class EvolutionTypedSerializerSpec extends FreeSpec with Matchers {
  val interpreter = new EvolutionTypedSerializer
  import interpreter.bind._, interpreter.chain.{ empty => emptyEvolution }, interpreter._, interpreter.constants._

  "EvolutionTypedSerializer" - {
    "should serialize with type information" - {
      "evolutions" - {
        "an empty evolution of doubles" in {
          emptyEvolution.infer(evolutionOfDoubles).toString shouldBe "empty: F[Double]"
        }

        "an empty evolution of points" in {
          emptyEvolution.infer(evolutionOfPoints).toString shouldBe "empty: F[Point]"
        }
      }

      "constants" - {
        "an app that returns a double" in {
          val expr = app[Point, Double](lambda("p", double(2)), point(double(0), double(0)))
          val expected =
            "app((p: Point -> 2.0: Double): Point -> Double, point(0.0: Double, 0.0: Double): Point): Double"
          expr.infer(doubleConstant).toString shouldBe expected
        }

        "a point" in {
          point(double(0), double(1)).infer(pointConstant).toString shouldBe "point(0.0: Double, 1.0: Double): Point"
        }
      }
    }
  }
}
