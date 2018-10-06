package evolution.primitive.algebra.binding

import cats.{Defer, MonoidK}
import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import evolution.primitive.algebra.binding.parser.{BindingAlgebraGrammar, BindingAlgebraSyntax, Expressions}
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Matchers}

class BindingAlgebraSyntaxSpec extends FreeSpec with Matchers with PrimitiveParsers with TestInterpreters {
  import ParserConfig.White._
  import fastparse.noApi._

  lazy val interpreter: BindingAlgebra[Binding, String] = BindingAlgebraTestInterpreter
  import interpreter._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0)($x)"
        val expected: Binding[Double] = let[Double, Double]("x", 10.0)(var0)
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        val expected: Binding[Double] =
          let("x", let("y", double(1.0))(var0))(let("z", double(2.0))(shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        val expected: Binding[Double] =
          let("x", let("y", double(1.0))(var0))(let("z", let("u", var0)(var0))(shift(var0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "inside a lambda" in {
        val serializedExpression = "app(lambda(x)(let(y, 1.0)($x)), 1.0)"
        val expected: Binding[Double] = app(lambda("x", let("y", double(1.0))(shift(var0))), double(1.0))
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
        val expected: Binding[Double => Double => Double] = lambda("x", lambda("y", shift(var0)))
        unsafeParseHOLambda(serializedExpression) shouldBe expected
      }

      "applications of HO lambdas" in {
        val serializedExpression = "app(lambda(x)(lambda(y)($x)), 1.0)"
        val expected: Binding[Double => Double] =
          app(lambda("x", lambda("y", shift(var0[Double]))), double(1.0))
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
        val expected: Binding[Double] = fix(lambda("x", double(1.0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "identities" in {
        val serializedExpression = "fix(lambda(x)($x))"
        val expected: Binding[Double] = fix(lambda("x", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      // We need to make this work
      "fixed points of HOF" in {
        val serializedExpression = "fix(lambda(f)($f))"
        val expected: Binding[Double => Double] = fix(lambda[Double => Double, Double => Double]("f", var0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }
  }
  val byVarParserMonoidK: MonoidK[ByVarParser[Binding, ?]] = new MonoidK[ByVarParser[Binding, ?]] {
    override def empty[A]: ByVarParser[Binding, A] = _ => Fail
    override def combineK[A](x: ByVarParser[Binding, A], y: ByVarParser[Binding, A]): ByVarParser[Binding, A] =
      vars => P(x(vars) | y(vars))
  }

  def double(d: Double): Binding[Double] = Lift[Double](d)
  val doubleParser: ByVarParser[Binding, Double] = _ => doubleLiteral.map(d => Lift[Double](d))

  val syntax =
    new BindingAlgebraSyntax[Binding](interpreter)

  def grammar(self: Expressions[ByVarParser[Binding, ?]]): Expressions[ByVarParser[Binding, ?]] =
    new BindingAlgebraGrammar[ByVarParser[Binding, ?], Parser[String]](
      self,
      syntax,
      syntax.variableIdentifier,
      byVarParserMonoidK,
      List(doubleParser)
    )

  val byVarParserDefer: Defer[ByVarParser[Binding, ?]] = new Defer[ByVarParser[Binding, ?]] {
    override def defer[A](fa: => ByVarParser[Binding, A]): ByVarParser[Binding, A] = vars => P(fa(vars))
  }

  val expressions2: Expressions[ByVarParser[Binding, ?]] =
    BindingAlgebraGrammar
      .fixMultipleExpressions[ByVarParser[Binding, ?]](byVarParserMonoidK, byVarParserDefer, List(grammar))

  val expressions: Expressions[ByVarParser[Binding, ?]] =
    BindingAlgebraGrammar
      .fixMultipleExpressions[ByVarParser[Binding, ?]](byVarParserMonoidK, byVarParserDefer, List(grammar))

  private def unsafeParseDouble(expression: String): Binding[Double] =
    expressions.value(doubleParser)(Nil).parse(expression).get.value

  private def unsafeParseLambda(expression: String): Binding[Double => Double] =
    expressions.func(expressions.value(doubleParser), expressions.value(doubleParser))(Nil).parse(expression).get.value

  private def unsafeParseHOLambda(expression: String): Binding[Double => Double => Double] =
    expressions
      .func(
        expressions.value(doubleParser),
        expressions.func(expressions.value(doubleParser), expressions.value(doubleParser))
      )(Nil)
      .parse(expression)
      .get
      .value
}
