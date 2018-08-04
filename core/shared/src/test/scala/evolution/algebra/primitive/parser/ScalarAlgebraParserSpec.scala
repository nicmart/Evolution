package evolution.algebra.primitive.parser

import evolution.geometry.Point
import evolution.primitive.algebra.ScalarAlgebra
import evolution.primitive.algebra.parser.{ParserConfig, ScalarAlgebraParser, ScalarParserContainer}
import org.scalatest.{FreeSpec, Matchers}
import evolution.primitive.algebra.parser.ParsersContainerOps._

class ScalarAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "A ScalarAlgebraParser should parse" - {
    "double literals" in {
      val serializedExpression = "1.0"
      unsafeParse(serializedExpression, container.parser[Scalar[Double]]) shouldBe DoubleScalar(1.0)
    }

    "negative double literals" in {
      val serializedExpression = "-2.0"
      unsafeParse(serializedExpression, container.parser[Scalar[Double]]) shouldBe DoubleScalar(-2.0)
    }

    "point literals" in {
      val serializedExpression = "point(1.0, -2.51)"
      unsafeParse(serializedExpression, container.parser[Scalar[Point]]) shouldBe PointScalar(Point(1.0, -2.51))
    }
  }

  object ScalarTestInterpreter extends ScalarAlgebra[Scalar] {
    override def double(d: Double): Scalar[Double] = DoubleScalar(d)
    override def point(point: Point): Scalar[Point] = PointScalar(point)
  }

  sealed trait Scalar[A]
  case class DoubleScalar(double: Double) extends Scalar[Double]
  case class PointScalar(point: Point) extends Scalar[Point]

  lazy val parser = new ScalarAlgebraParser(ScalarTestInterpreter)

  lazy val container: ScalarParserContainer[Scalar] =
    parser.buildContainer(ScalarParserContainer.empty[Scalar])
}
