package evolution.primitive.algebra.binding

import cats.{Defer, MonoidK}
import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import evolution.primitive.parser.CommonTestParsers
import evolution.primitive.algebra.binding.parser.{BindingAlgebraGrammar, BindingAlgebraSyntax, Expressions}
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Matchers}

class BindingAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers with TestInterpreters {
  import ParserConfig.White._
  import fastparse.noApi._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0)($x)"
        val expected: Binding[Double] = Let[Double, Double]("x", 10.0, Var0())
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        val expected: Binding[Double] =
          Let("x", Let("y", Lift[Double](1.0), Var0()), Let("z", Lift[Double](2.0), Shift(Var0())))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        val expected: Binding[Double] =
          Let("x", Let("y", Lift[Double](1.0), Var0()), Let("z", Let("u", Var0(), Var0()), Shift(Var0())))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "inside a lambda" in {
        val serializedExpression = "app(lambda(x)(let(y, 1.0)($x)), 1.0)"
        val expected: Binding[Double] = App(Lambda("x", Let("y", Lift[Double](1.0), Shift(Var0()))), Lift[Double](1.0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }

    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "lambda(x)(1.0)"
        val expected = Lambda("x", Lift[Double](1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "identity" in {
        val serializedExpression = "lambda(x)($x)"
        val expected = Lambda("x", Var0())
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "lambda(x)(lambda(y)($x))"
        val expected: Binding[Double => Double => Double] = Lambda("x", Lambda("y", Shift(Var0())))
        unsafeParseHOLambda(serializedExpression) shouldBe expected
      }

      "applications of HO lambdas" in {
        val serializedExpression = "app(lambda(x)(lambda(y)($x)), 1.0)"
        val expected: Binding[Double => Double] =
          App(Lambda("x", Lambda("y", Shift(Var0[Double]()))), Lift[Double](1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0)(lambda(y)($x))"
        val expected: Binding[Double => Double] = Let("x", Lift[Double](1.0), Lambda("y", Shift(Var0())))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(lambda(x)(1.0))"
        val expected: Binding[Double] = Fix(Lambda("x", Lift[Double](1.0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "identities" in {
        val serializedExpression = "fix(lambda(x)($x))"
        val expected: Binding[Double] = Fix(Lambda("x", Var0()))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      // We need to make this work
      "fixed points of HOF" in {
        val serializedExpression = "fix(lambda(f)($f))"
        val expected: Binding[Double => Double] = Fix(Lambda[Double => Double, Double => Double]("f", Var0()))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }
  }
  val byVarParserMonoidK: MonoidK[ByVarParser[Binding, ?]] = new MonoidK[ByVarParser[Binding, ?]] {
    override def empty[A]: ByVarParser[Binding, A] = _ => Fail
    override def combineK[A](x: ByVarParser[Binding, A], y: ByVarParser[Binding, A]): ByVarParser[Binding, A] =
      vars => P(x(vars) | y(vars))
  }

  val doubleParser: ByVarParser[Binding, Double] = _ => double.map(d => Lift[Double](d))

  val syntax: BindingAlgebra[ByVarParser[Binding, ?], Parser[String]] =
    new BindingAlgebraSyntax[Binding](BindingAlgebraTestInterpreter)

  val varNameSyntax: Parser[String] = varName

  def grammar(self: Expressions[ByVarParser[Binding, ?]]): Expressions[ByVarParser[Binding, ?]] =
    new BindingAlgebraGrammar[ByVarParser[Binding, ?], Parser[String]](
      self,
      syntax,
      varNameSyntax,
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
