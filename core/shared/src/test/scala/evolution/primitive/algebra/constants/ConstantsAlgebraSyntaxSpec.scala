package evolution.primitive.algebra.constants

import cats.implicits._
import cats.kernel.Semigroup
import cats.{Defer, MonoidK}
import evolution.primitive.parser.CommonTestParsers
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, TestInterpreters}
import evolution.primitive.algebra.constants.parser._
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi.{Fail, P, Parser}
import org.scalatest.{FreeSpec, Matchers}

class ConstantsAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers with TestInterpreters {

  "A ScalarAlgebraParser should parse" - {
    "double literals" in {
      val serializedExpression = "1.0"
      unsafeParseDouble(serializedExpression) shouldBe Value(1.0)
    }

    "negative double literals" in {
      val serializedExpression = "-2.0"
      unsafeParseDouble(serializedExpression) shouldBe Value(-2.0)
    }

    "point literals" in {
      val serializedExpression = "point(1.0, -2.51)"
      unsafeParsePoint(serializedExpression) shouldBe Value(Point(1.0, -2.51))
    }

    "sum of doubles" in {
      val serializedExpression = "add(1.0, 2.0)"
      unsafeParseDouble(serializedExpression) shouldBe Add(1.0, 2.0)
    }

    "sum of points" in {
      val serializedExpression = "add(point(1.0, 2.0), point(3, 4))"
      unsafeParsePoint(serializedExpression) shouldBe Add(Point(1.0, 2.0), Point(3.0, 4.0))
    }
  }

  type ConstantParser[T] = Parser[Binding[Constant[T]]]
  type TestExpressions = Expressions[ConstantParser]

  val doubleParser: Parser[Binding[Constant[Double]]] =
    doubleLiteral.map { d =>
      Lift(Value(d))
    }

  val pointParser: Parser[Binding[Constant[Point]]] =
    function2("point", doubleLiteral, doubleLiteral).map { case (x, y) => Lift(Value(Point(x, y))) }

  // TODO move somewhere
  lazy val parserMonoidK: MonoidK[ConstantParser] = new MonoidK[ConstantParser] {
    override def empty[A]: ConstantParser[A] = Fail
    override def combineK[A](x: ConstantParser[A], y: ConstantParser[A]): ConstantParser[A] = P(x | y)
  }

  lazy val deferParser: Defer[ConstantParser] = new Defer[ConstantParser] {
    override def defer[A](fa: => ConstantParser[A]): ConstantParser[A] = P(fa)
  }

  val syntax: ConstantsAlgebraSyntax[Composed[Binding, Constant, ?]] =
    new ConstantsAlgebraSyntax[Composed[Binding, Constant, ?]](ConstantsAlgebraTestInterpreter)

  def grammar(self: Expressions[ConstantParser]): Expressions[ConstantParser] =
    new ConstantsAlgebraGrammar[ConstantParser](self, syntax, parserMonoidK)

  def expressions: Expressions[ConstantParser] =
    grammar(new LazyExpressions[ConstantParser](expressions, deferParser))

  private def unsafeParseDouble(serializedExpression: String): Constant[Double] =
    expressions.get(doubleParser).parse(serializedExpression).get.value match {
      case Lift(inner) => inner
    }
  private def unsafeParsePoint(serializedExpression: String): Constant[Point] =
    expressions.get(pointParser).parse(serializedExpression).get.value match {
      case Lift(inner) => inner
    }
}
