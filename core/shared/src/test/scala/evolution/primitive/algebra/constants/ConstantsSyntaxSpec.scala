package evolution.primitive.algebra.constants

import cats.implicits._
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, Composed, TestInterpreters}
import evolution.primitive.algebra.constants.parser._
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.parser.{ConstantsExpressions, EvolutionGrammar}
import org.scalatest.{FreeSpec, Matchers}

class ConstantsSyntaxSpec extends FreeSpec with Matchers with TestInterpreters {
  val interpreter: Evolution[Constant, ListExpr, Binding, Double, String] = EvolutionAlgebraTestInterpreter
  import interpreter.constants._

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

  type BindingParser[T] = ByVarParser[Binding, T]

  def expressions: ConstantsExpressions[Constant, BindingParser] =
    EvolutionGrammar.grammar(interpreter).constants

  private def unsafeParseDouble(serializedExpression: String): Binding[Constant[Double]] = {
    expressions.constantOf(expressions.doubles)(Semigroup[Double])(Nil).parse(serializedExpression).get.value
  }

  private def unsafeParsePoint(serializedExpression: String): Binding[Constant[Point]] =
    expressions.constantOf(expressions.points)(Semigroup[Point])(Nil).parse(serializedExpression).get.value
}
