package evolution.algebra.primitive.parser

import cats.{Defer, MonoidK}
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.{ScalarAlgebra, parser}
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import evolution.primitive.algebra.parser.PrimitiveParsers.function2
import evolution.primitive.algebra.parser.{LazyExpressions, ParserConfig, ScalarAlgebra}
import fastparse.noApi.{Fail, P, Parser}
import fastparse.noApi._
import fastparse.noApi
import fastparse.all
import ParserConfig.White._
import fastparse.noApi._

class ScalarAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers {

  "A ScalarAlgebraParser should parse" - {
    "double literals" in {
      val serializedExpression = "1.0"
      unsafeParseDouble(serializedExpression) shouldBe DoubleScalar(1.0)
    }

    "negative double literals" in {
      val serializedExpression = "-2.0"
      unsafeParseDouble(serializedExpression) shouldBe DoubleScalar(-2.0)
    }

    "point literals" in {
      val serializedExpression = "point(1.0, -2.51)"
      unsafeParsePoint(serializedExpression) shouldBe PointScalar(Point(1.0, -2.51))
    }

    "sum of doubles" in {
      val serializedExpression = "add(1.0, 2.0)"
      unsafeParseDouble(serializedExpression) shouldBe Add(DoubleScalar(1.0), DoubleScalar(2))
    }

    "sum of points" in {
      val serializedExpression = "add(point(1.0, 2.0), point(3, 4))"
      unsafeParsePoint(serializedExpression) shouldBe Add(PointScalar(Point(1.0, 2)), PointScalar(Point(3, 4)))
    }
  }

  type ScalarParser[T] = Parser[Scalar[T]]
  type TestExpressions = ScalarAlgebra.Expressions[ScalarParser]

  object ScalarTestInterpreter extends ScalarAlgebra[Scalar] {
    override def double(d: Double): Scalar[Double] = DoubleScalar(d)
    override def point(x: Double, y: Double): Scalar[Point] = PointScalar(Point(x, y))
    override def add[T: Semigroup](a: Scalar[T], b: Scalar[T]): Scalar[T] = Add(a, b)
  }

  sealed trait Scalar[A]
  case class DoubleScalar(double: Double) extends Scalar[Double]
  case class PointScalar(point: Point) extends Scalar[Point]
  case class Add[T: Semigroup](a: Scalar[T], b: Scalar[T]) extends Scalar[T]

  val doubleParser: Parser[Scalar[Double]] =
    double.map { d =>
      DoubleScalar(d)
    }

  val pointParser: Parser[Scalar[Point]] =
    function2("point", double, double).map { case (x, y) => PointScalar(Point(x, y)) }

  // TODO move somewhere
  lazy val parserMonoidK: MonoidK[ScalarParser] = new MonoidK[ScalarParser] {
    override def empty[A]: ScalarParser[A] = Fail
    override def combineK[A](x: ScalarParser[A], y: ScalarParser[A]): ScalarParser[A] = P(x | y)
  }

  lazy val deferParser: Defer[ScalarParser] = new Defer[ScalarParser] {
    override def defer[A](fa: => ScalarParser[A]): ScalarParser[A] = P(fa)
  }

  val syntax: ScalarAlgebra.Syntax[Scalar] = new ScalarAlgebra.Syntax(ScalarTestInterpreter)

  def grammar(self: ScalarAlgebra.Expressions[ScalarParser]): ScalarAlgebra.Expressions[ScalarParser] =
    new ScalarAlgebra.Grammar[ScalarParser](self, syntax, parserMonoidK)

  def expressions: ScalarAlgebra.Expressions[ScalarParser] =
    grammar(new parser.ScalarAlgebra.LazyExpressions[ScalarParser](expressions, deferParser))

  private def unsafeParseDouble(serializedExpression: String): Scalar[Double] =
    expressions.get(doubleParser).parse(serializedExpression).get.value
  private def unsafeParsePoint(serializedExpression: String): Scalar[Point] =
    expressions.get(pointParser).parse(serializedExpression).get.value
}
