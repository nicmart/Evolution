package evolution.algebra.primitive.parser

import cats.{Defer, MonoidK}
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.binding.parser.{BindingAlgebraGrammar, BindingAlgebraSyntax, Expressions}
import evolution.primitive.algebra.parser._
import fastparse.noApi.Parser
import org.scalatest.{FreeSpec, Matchers}

class BindingAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import fastparse.noApi._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0)($x)"
        val expected: Expr[Double] = Let("x", Value(10.0), Var0())
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        val expected: Expr[Double] = Let("x", Let("y", Value(1.0), Var0()), Let("z", Value(2.0), Shift(Var0())))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        val expected: Expr[Double] =
          Let("x", Let("y", Value(1.0), Var0()), Let("z", Let("u", Var0(), Var0()), Shift(Var0())))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "inside a lambda" in {
        val serializedExpression = "app(lambda(x)(let(y, 1.0)($x)), 1.0)"
        val expected: Expr[Double] = App(Lambda("x", Let("y", Value(1.0), Shift(Var0()))), Value(1.0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }

    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "lambda(x)(1.0)"
        val expected = Lambda("x", Value(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "identity" in {
        val serializedExpression = "lambda(x)($x)"
        val expected = Lambda("x", Var0())
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "nested" in {
        val serializedExpression = "lambda(x)(lambda(y)($x))"
        val expected: Expr[Double => Double => Double] = Lambda("x", Lambda("y", Shift(Var0())))
        unsafeParseHOLambda(serializedExpression) shouldBe expected
      }

      "applications of HO lambdas" in {
        val serializedExpression = "app(lambda(x)(lambda(y)($x)), 1.0)"
        val expected: Expr[Double => Double] = App(Lambda("x", Lambda("y", Shift(Var0[Double]()))), Value(1.0))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0)(lambda(y)($x))"
        val expected: Expr[Double => Double] = Let("x", Value(1.0), Lambda("y", Shift(Var0())))
        unsafeParseLambda(serializedExpression) shouldBe expected
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(lambda(x)(1.0))"
        val expected: Expr[Double] = Fix(Lambda("x", Value(1.0)))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "identities" in {
        val serializedExpression = "fix(lambda(x)($x))"
        val expected: Expr[Double] = Fix(Lambda("x", Var0()))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      // We need to make this work
      "fixed points of HOF" in {
        val serializedExpression = "fix(lambda(f)($f))"
        val expected: Expr[Double => Double] = Fix(Lambda[Double => Double, Double => Double]("f", Var0()))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }
    }
  }

  sealed trait Expr[A]
  case class Value[A](a: A) extends Expr[A]
  case class Var0[A]() extends Expr[A]
  case class Shift[A](expr: Expr[A]) extends Expr[A]
  case class Let[A, B](name: String, value: Expr[A], expr: Expr[B]) extends Expr[B]
  case class Lambda[A, B](name: String, expr: Expr[B]) extends Expr[A => B]
  case class App[A, B](f: Expr[A => B], a: Expr[A]) extends Expr[B]
  case class Fix[A](expr: Expr[A => A]) extends Expr[A]

  type ByVarParser[A] = List[String] => Parser[Expr[A]]

  val byVarParserMonoidK: MonoidK[ByVarParser] = new MonoidK[ByVarParser] {
    override def empty[A]: ByVarParser[A] = _ => Fail
    override def combineK[A](x: ByVarParser[A], y: ByVarParser[A]): ByVarParser[A] = vars => P(x(vars) | y(vars))
  }

  val testInterpreter: BindingAlgebra[Expr, String] = new BindingAlgebra[Expr, String] {
    override def varName(name: String): String = name
    override def var0[A]: Expr[A] = Var0[A]()
    override def shift[A](expr: Expr[A]): Expr[A] = Shift(expr)
    override def let[A, B](name: String, value: Expr[A])(expr: Expr[B]): Expr[B] = Let(name, value, expr)
    override def lambda[A, B](name: String, expr: Expr[B]): Expr[A => B] = Lambda(name, expr)
    override def app[A, B](f: Expr[A => B], a: Expr[A]): Expr[B] = App(f, a)
    override def fix[A](expr: Expr[A => A]): Expr[A] = Fix(expr)
  }

  val doubleParser: ByVarParser[Double] = _ => double.map(d => Value[Double](d))

  val syntax: BindingAlgebra[ByVarParser, Parser[String]] =
    new BindingAlgebraSyntax[Expr](testInterpreter)

  val varNameSyntax: Parser[String] = varName

  def grammar(self: Expressions[ByVarParser]): Expressions[ByVarParser] =
    new BindingAlgebraGrammar[ByVarParser, Parser[String]](
      self,
      syntax,
      varNameSyntax,
      byVarParserMonoidK,
      List(doubleParser)
    )

  val byVarParserDefer: Defer[ByVarParser] = new Defer[ByVarParser] {
    override def defer[A](fa: => ByVarParser[A]): ByVarParser[A] = vars => P(fa(vars))
  }

  val expressions2: Expressions[ByVarParser] =
    BindingAlgebraGrammar
      .fixMultipleExpressions[ByVarParser](byVarParserMonoidK, byVarParserDefer, List(grammar))

  val expressions: Expressions[ByVarParser] =
    BindingAlgebraGrammar
      .fixMultipleExpressions[ByVarParser](byVarParserMonoidK, byVarParserDefer, List(grammar))

  private def unsafeParseDouble(expression: String): Expr[Double] =
    expressions.value(doubleParser)(Nil).parse(expression).get.value

  private def unsafeParseLambda(expression: String): Expr[Double => Double] =
    expressions.func(expressions.value(doubleParser), expressions.value(doubleParser))(Nil).parse(expression).get.value

  private def unsafeParseHOLambda(expression: String): Expr[Double => Double => Double] =
    expressions
      .func(
        expressions.value(doubleParser),
        expressions.func(expressions.value(doubleParser), expressions.value(doubleParser))
      )(Nil)
      .parse(expression)
      .get
      .value
}
