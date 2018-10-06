package evolution.primitive.algebra.constants

import cats.implicits._
import cats.{Defer, MonoidK}
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, TestInterpreters}
import evolution.primitive.algebra.constants.parser._
import evolution.primitive.algebra.parser.ParserConfig.White._
import fastparse.noApi.{Fail, P, Parser}
import org.scalatest.{FreeSpec, Matchers}
import evolution.primitive.algebra.parser.PrimitiveParsers.doubleLiteral

class ConstantsAlgebraSyntaxSpec extends FreeSpec with Matchers with TestInterpreters {
  val interpreter: ConstantsAlgebra[Composed[Binding, Constant, ?]] =
    ConstantsAlgebraTestInterpreter
  import interpreter._

  "A ScalarAlgebraParser should parse" - {
    "double literals" in {
      val serializedExpression = "1.0"
      unsafeParseDouble(serializedExpression) shouldBe double(1)
    }

    "negative double literals" in {
      val serializedExpression = "-2.0"
      unsafeParseDouble(serializedExpression) shouldBe double(-2)
    }

    "point literals" in {
      val serializedExpression = "point(1.0, -2.51)"
      unsafeParsePoint(serializedExpression) shouldBe point(double(1.0), double(-2.51))
    }

    "sum of doubles" in {
      val serializedExpression = "add(1.0, 2.0)"
      unsafeParseDouble(serializedExpression) shouldBe add(double(1.0), double(2.0))
    }

    "sum of points" in {
      val serializedExpression = "add(point(1.0, 2.0), point(3, 4))"
      unsafeParsePoint(serializedExpression) shouldBe add(point(double(1), double(2)), point(double(3), double(4)))
    }
  }

  type ConstantParser[T] = Parser[Binding[Constant[T]]]
  type TestExpressions = Expressions[ConstantParser]

  val doubleParser: Parser[Binding[Constant[Double]]] =
    doubleLiteral.map { d =>
      Lift(Value(d))
    }

  // TODO move somewhere
  lazy val parserMonoidK: MonoidK[ConstantParser] = new MonoidK[ConstantParser] {
    override def empty[A]: ConstantParser[A] = Fail
    override def combineK[A](x: ConstantParser[A], y: ConstantParser[A]): ConstantParser[A] = P(x | y)
  }

  lazy val deferParser: Defer[ConstantParser] = new Defer[ConstantParser] {
    override def defer[A](fa: => ConstantParser[A]): ConstantParser[A] = P(fa)
  }

  val syntax: ConstantsAlgebraSyntax[Composed[Binding, Constant, ?]] =
    new ConstantsAlgebraSyntax[Composed[Binding, Constant, ?]](interpreter)

  def grammar(self: Expressions[ConstantParser]): Expressions[ConstantParser] =
    new ConstantsAlgebraGrammar[ConstantParser](self, syntax, parserMonoidK)

  def expressions: Expressions[ConstantParser] =
    grammar(new LazyExpressions[ConstantParser](expressions, deferParser))

  private def unsafeParseDouble(serializedExpression: String): Binding[Constant[Double]] =
    expressions.get(doubleParser).parse(serializedExpression).get.value

  private def unsafeParsePoint(serializedExpression: String): Binding[Constant[Point]] =
    expressions.get(syntax.anyPoint).parse(serializedExpression).get.value
}
