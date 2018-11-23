package evolution.primitive.algebra.constants

import cats.implicits._
import evolution.geometry.Point
import evolution.primitive.algebra.{ Composed, TestInterpreters }
import evolution.primitive.algebra.constants.parser._
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.parser.{ Expressions, EvolutionGrammar }
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi
import org.scalatest.{ FreeSpec, Matchers }

class ConstantsParserSyntaxSpec extends FreeSpec with Matchers with TestInterpreters {
  val interpreter: Evolution[ListExpr, Binding, Double, String, String] = EvolutionAlgebraTestInterpreter
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

  type BindingParser[T] = ByVarParserK[Binding, T]

  def expressions: Expressions[ListExpr, ByVarParserK[Binding, ?], noApi.Parser[String]] =
    EvolutionGrammar.parserGrammar(interpreter)

  private def unsafeParseDouble(serializedExpression: String): Binding[Double] = {
    expressions.doubleConstant.parser(Nil).parse(serializedExpression).get.value
  }

  private def unsafeParsePoint(serializedExpression: String): Binding[Point] =
    expressions.pointConstant.parser(Nil).parse(serializedExpression).get.value
}
