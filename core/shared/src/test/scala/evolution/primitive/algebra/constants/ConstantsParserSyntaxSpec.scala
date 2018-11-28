package evolution.primitive.algebra.constants

import evolution.primitive.algebra.evolution.interpreter.Types.{ doubleConstant, pointConstant }
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionTypedSerializer, Types }
import evolution.primitive.algebra.evolution.parser.{ EvolutionGrammar, Expressions }
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi
import org.scalatest.{ FreeSpec, Matchers }

class ConstantsParserSyntaxSpec extends FreeSpec with Matchers {
  val interpreter = new EvolutionTypedSerializer
  import interpreter.constants._

  "A ScalarAlgebraParser should parse" - {
    "double literals" in {
      val serializedExpression = "1.0"
      unsafeParseDouble(serializedExpression) shouldBe double(1).infer(doubleConstant).toString
    }

    "negative double literals" in {
      val serializedExpression = "-2.0"
      unsafeParseDouble(serializedExpression) shouldBe double(-2).infer(doubleConstant).toString
    }

    "point literals" in {
      val serializedExpression = "point(1.0, -2.51)"
      unsafeParsePoint(serializedExpression) shouldBe point(double(1.0), double(-2.51)).infer(pointConstant).toString
    }

    "sum of doubles" in {
      val serializedExpression = "add(1.0, 2.0)"
      unsafeParseDouble(serializedExpression) shouldBe add(double(1.0), double(2.0)).infer(doubleConstant).toString
    }

    "sum of points" in {
      val serializedExpression = "add(point(1.0, 2.0), point(3, 4))"
      unsafeParsePoint(serializedExpression) shouldBe add(point(double(1), double(2)), point(double(3), double(4)))
        .infer(pointConstant)
        .toString
    }
  }

  def expressions: Expressions[Types.F, ByVarParserK[Types.R, ?]] =
    EvolutionGrammar.parserGrammar(interpreter)

  private def unsafeParseDouble(serializedExpression: String): String =
    expressions.doubleConstant.parser(Nil).parse(serializedExpression).get.value.infer(doubleConstant).toString

  private def unsafeParsePoint(serializedExpression: String): String =
    expressions.pointConstant.parser(Nil).parse(serializedExpression).get.value.infer(pointConstant).toString
}
