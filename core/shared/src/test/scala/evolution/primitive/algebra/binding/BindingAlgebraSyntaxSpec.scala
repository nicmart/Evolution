package evolution.primitive.algebra.binding

import cats.kernel.Semigroup
import cats.{Defer, MonoidK}
import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import evolution.primitive.algebra.binding.parser.{BindingAlgebraGrammar, BindingAlgebraSyntax, Expressions}
import cats.implicits._
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.evolution.parser.{EvolutionAlgebraExpressions, EvolutionAlgebraGrammar}
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Matchers}

class BindingAlgebraSyntaxSpec extends FreeSpec with Matchers with PrimitiveParsers with TestInterpreters {
  import ParserConfig.White._
  import fastparse.noApi._

  val interpreter: EvolutionAlgebra[Constant, ListExpr, Binding, String] = EvolutionAlgebraTestInterpreter
  import interpreter.bind._, interpreter.constants._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0)($x)"
        val expected: Binding[Constant[Double]] = let[Constant[Double], Constant[Double]]("x", 10.0)(var0)
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        val expected: Binding[Constant[Double]] =
          let("x", let("y", double(1.0))(var0))(let("z", double(2.0))(shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        val expected: Binding[Constant[Double]] =
          let("x", let("y", double(1.0))(var0))(let("z", let("u", var0)(var0))(shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "inside a lambda" in {
        val serializedExpression = "app(lambda(x)(let(y, 1.0)($x)), 1.0)"
        val expected: Binding[Constant[Double]] = app(lambda("x", let("y", double(1.0))(shift(var0))), double(1.0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }

    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "lambda(x)(1.0)"
        val expected = lambda("x", double(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "identity" in {
        val serializedExpression = "lambda(x)($x)"
        val expected = lambda("x", var0)
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "lambda(x)(lambda(y)($x))"
        val expected: Binding[Constant[Double] => Constant[Double] => Constant[Double]] =
          lambda("x", lambda("y", shift(var0)))
        unsafeParseHOLambda(serializedExpression) shouldBe expected
      }

      "applications of HO lambdas" in {
        val serializedExpression = "app(lambda(x)(lambda(y)($x)), 1.0)"
        val expected: Binding[Constant[Double] => Constant[Double]] =
          app(lambda("x", lambda("y", shift(var0[Constant[Double]]))), double(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0)(lambda(y)($x))"
        val expected: Binding[Double => Double] = let("x", double(1.0))(lambda("y", shift(var0)))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(lambda(x)(1.0))"
        val expected: Binding[Constant[Double]] = fix(lambda("x", double(1.0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "identities" in {
        val serializedExpression = "fix(lambda(x)($x))"
        val expected: Binding[Constant[Double]] = fix(lambda("x", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      // We need to make this work
      "fixed points of HOF" in {
        val serializedExpression = "fix(lambda(f)($f))"
        val expected: Binding[Constant[Double] => Constant[Double]] =
          fix(lambda[Constant[Double] => Constant[Double], Constant[Double] => Constant[Double]]("f", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }
  }

  type BindingParser[T] = ByVarParser[Binding, T]

  def expressions: EvolutionAlgebraExpressions[Constant, ListExpr, BindingParser] =
    EvolutionAlgebraGrammar.grammar(EvolutionAlgebraTestInterpreter)

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
