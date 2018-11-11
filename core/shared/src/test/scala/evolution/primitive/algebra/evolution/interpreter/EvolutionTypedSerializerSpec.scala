package evolution.primitive.algebra.evolution.interpreter
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.interpreter.Types.{ F, TypeInfo }
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionTypedSerializerSpec extends FreeSpec with Matchers {
  val interpreter = new EvolutionTypedSerializer
  import interpreter.bind._, interpreter.chain.{ empty => emptyEvolution }, interpreter._, interpreter.constants._

  "EvolutionTypedSerializer" - {
    "should serialize with type information" - {
      "an empty evolution of doubles" in {
        emptyEvolution(evolutionOfDoubles) shouldBe "empty:F[Double]"
      }

      "an empty evolution of points" in {
        emptyEvolution(evolutionOfPoints) shouldBe "empty:F[Point]"
      }

      "an app that returns a double" in {
        val expr = app[Point, Double](lambda("p", double(2)), point(double(0), double(0)))
        expr(doubleConstant) shouldBe "app(p:Point->2:Double):Double"
      }
    }
  }

  lazy val evolutionOfDoubles: TypeInfo[F[Double]] = TypeInfo("F[Double]")
  lazy val evolutionOfPoints: TypeInfo[F[Double]] = TypeInfo("F[Point]")
  lazy val doubleConstant: TypeInfo[Double] = TypeInfo("Double")
  lazy val pointConstant: TypeInfo[Double] = TypeInfo("Point")
}
