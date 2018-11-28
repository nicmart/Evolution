package evolution.primitive.algebra.binding.parser

import evolution.primitive.algebra.evolution.interpreter.Types.{ FunctionTypeInfo, doubleConstant }
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionTypedSerializer, Types }
import evolution.primitive.algebra.evolution.parser.{ EvolutionGrammar, Expressions }
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.primitive.algebra.parser._
import fastparse.noApi
import org.scalatest.{ FreeSpec, Matchers }

class BindingParserSyntaxSpec extends FreeSpec with Matchers {

  val interpreter = new EvolutionTypedSerializer
  import interpreter.bind._, interpreter.constants._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0, $x)"
        val expected = let[Double, Double]("x", double(10.0), var0[Double])
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0, $y), let(z, 2.0, $x))"
        val expected =
          let("x", let("y", double(1.0), var0), let("z", double(2.0), shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0, $y), let(z, let(u, $x, $u), $x))"
        val expected = let("x", let("y", double(1.0), var0), let("z", let("u", var0, var0), shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }

      "inside a lambda" in {
        val serializedExpression = "app(x -> let(y, 1.0, $x), 1.0)"
        val expected = app(lambda("x", let("y", double(1.0), shift(var0))), double(1.0))
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }
    }

    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "x -> 1.0"
        val expected = lambda("x", double(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected.infer(lambdaType).toString
      }

      "identity" in {
        val serializedExpression = "x -> $x"
        val expected = lambda("x", var0)
        unsafeParseLambda(serializedExpression) shouldBe expected.infer(lambdaType).toString
      }

      "nested" in {
        val serializedExpression = "x -> y -> $x"
        val expected =
          lambda("x", lambda("y", shift(var0)))
        unsafeParseHOLambda(serializedExpression) shouldBe expected.infer(lambdaHOType).toString
      }

      "applications of HO lambdas" in {
        val serializedExpression = "app(x -> y -> $x, 1.0)"
        val expected =
          app(lambda[Double, Double => Double]("x", lambda[Double, Double]("y", shift(var0[Double]))), double(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected.infer(lambdaType).toString
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0, y -> $x)"
        val expected = let("x", double(1.0), lambda("y", shift(var0)))
        unsafeParseLambda(serializedExpression) shouldBe expected.infer(lambdaType).toString
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(x -> 1.0)"
        val expected = fix[Double](lambda("x", double(1.0)))
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }

      "identities" in {
        val serializedExpression = "fix(x -> $x)"
        val expected = fix(lambda("x", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }

      // We need to make this work
      "fixed points of HOF" in {
        val serializedExpression = "fix(f -> $f)"
        val expected =
          fix(lambda[Double => Double, Double => Double]("f", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected.infer(doubleConstant).toString
      }
    }
  }

  def expressions: Expressions[Types.F, ByVarParserK[Types.R, ?]] =
    EvolutionGrammar.parserGrammar(new EvolutionTypedSerializer)

  private def unsafeParseDouble(expression: String): String =
    expressions.doubleConstant.parser(Nil).parse(expression).get.value.infer(doubleConstant).toString

  lazy val lambdaType = FunctionTypeInfo(doubleConstant, doubleConstant)
  lazy val lambdaHOType = FunctionTypeInfo(doubleConstant, lambdaType)

  private def unsafeParseLambda(expression: String): String =
    expressions
      .function(expressions.doubleConstant, expressions.doubleConstant)
      .parser(Nil)
      .parse(expression)
      .get
      .value
      .infer(lambdaType)
      .toString

  private def unsafeParseHOLambda(expression: String): String =
    expressions
      .function(
        expressions.doubleConstant,
        expressions.function(
          expressions.doubleConstant,
          expressions.doubleConstant
        )
      )
      .parser(Nil)
      .parse(expression)
      .get
      .value
      .infer(lambdaHOType)
      .toString
}
