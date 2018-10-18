package evolution.primitive.algebra.binding

import cats.kernel.Semigroup
import cats.{Defer, MonoidK}
import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import cats.implicits._
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.parser.{EvolutionExpressions, EvolutionGrammar}
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Matchers}

class BindingSyntaxSpec extends FreeSpec with Matchers with PrimitiveParsers with TestInterpreters {
  import ParserConfig.White._
  import fastparse.noApi._

  val interpreter: Evolution[Constant, ListExpr, Binding, Double, String, String] = EvolutionAlgebraTestInterpreter
  import interpreter.bind._, interpreter.constants._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0, $x)"
        val expected: Binding[Constant[Double]] = let[Constant[Double], Constant[Double]]("x", 10.0, var0)
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0, $y), let(z, 2.0, $x))"
        val expected: Binding[Constant[Double]] =
          let("x", let("y", double(1.0), var0), let("z", double(2.0), shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0, $y), let(z, let(u, $x, $u), $x))"
        val expected: Binding[Constant[Double]] =
          let("x", let("y", double(1.0), var0), let("z", let("u", var0, var0), shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "inside a lambda" in {
        val serializedExpression = "app(x -> let(y, 1.0, $x), 1.0)"
        val expected: Binding[Constant[Double]] = app(lambda("x", let("y", double(1.0), shift(var0))), double(1.0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }

    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "x -> 1.0"
        val expected = lambda("x", double(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "identity" in {
        val serializedExpression = "x -> $x"
        val expected = lambda("x", var0)
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "x -> y -> $x"
        val expected: Binding[Constant[Double] => Constant[Double] => Constant[Double]] =
          lambda("x", lambda("y", shift(var0)))
        unsafeParseHOLambda(serializedExpression) shouldBe expected
      }

      "applications of HO lambdas" in {
        val serializedExpression = "app(x -> y -> $x, 1.0)"
        val expected: Binding[Constant[Double] => Constant[Double]] =
          app(lambda("x", lambda("y", shift(var0[Constant[Double]]))), double(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0, y -> $x)"
        val expected: Binding[Double => Double] = let("x", double(1.0), lambda("y", shift(var0)))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(x -> 1.0)"
        val expected: Binding[Constant[Double]] = fix(lambda("x", double(1.0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "identities" in {
        val serializedExpression = "fix(x -> $x)"
        val expected: Binding[Constant[Double]] = fix(lambda("x", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      // We need to make this work
      "fixed points of HOF" in {
        val serializedExpression = "fix(f -> $f)"
        val expected: Binding[Constant[Double] => Constant[Double]] =
          fix(lambda[Constant[Double] => Constant[Double], Constant[Double] => Constant[Double]]("f", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }
  }

  type BindingParser[T] = ByVarParser[Binding, T]

  def expressions: EvolutionExpressions[Constant, ListExpr, BindingParser] =
    EvolutionGrammar.grammar(EvolutionAlgebraTestInterpreter)

  private def unsafeParseDouble(expression: String): Binding[Constant[Double]] =
    expressions.binding.valueOf(expressions.constants.doubles)(Nil).parse(expression).get.value

  private def unsafeParseLambda(expression: String): Binding[Constant[Double] => Constant[Double]] =
    expressions.binding
      .function(
        expressions.binding.valueOf(expressions.constants.doubles),
        expressions.binding.valueOf(expressions.constants.doubles)
      )(Nil)
      .parse(expression)
      .get
      .value

  private def unsafeParseHOLambda(
    expression: String
  ): Binding[Constant[Double] => Constant[Double] => Constant[Double]] =
    expressions.binding
      .function(
        expressions.binding.valueOf(expressions.constants.doubles),
        expressions.binding
          .function(
            expressions.binding.valueOf(expressions.constants.doubles),
            expressions.binding.valueOf(expressions.constants.doubles)
          )
      )(Nil)
      .parse(expression)
      .get
      .value
}
