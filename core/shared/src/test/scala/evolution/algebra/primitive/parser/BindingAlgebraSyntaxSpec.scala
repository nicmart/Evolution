package evolution.algebra.primitive.parser

import cats.{Defer, MonoidK}
import evolution.drawing.algebra.interpreter.CtxString
import evolution.primitive.algebra.{BindingAlgebra, parser}
import evolution.primitive.algebra.interpreter.BindingAlgebraSerializer
import evolution.primitive.algebra.parser.{BindingAlgebra, _}
import fastparse.noApi
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
        val serializedExpression = "fix(1.0)"
        val expected: Expr[Double] = Fix(Value(1.0))
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "identities" in {
        val serializedExpression = "fix($self)"
        val expected: Expr[Double] = Fix(Var0())
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      // We need to make this work
      "fixed points of HOF" in {
        pending
        val serializedExpression = "app( , 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
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
  case class Fix[A](expr: Expr[A]) extends Expr[A]

  type ByVarParser[A] = List[String] => Parser[Expr[A]]

  trait Type[T]
  case object DoubleType extends Type[Double]

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
    override def fix[A](expr: Expr[A]): Expr[A] = Fix(expr)
  }

  val baseExpressions: BindingAlgebra.Expressions[ByVarParser, Type] =
    new BindingAlgebra.Expressions[ByVarParser, Type] {
      override def value[T](t: Type[T]): ByVarParser[T] = t match {
        case DoubleType =>
          _ =>
            double.map(d => Value[Double](d))
      }
      override def func[T1, T2](t1: Type[T1], t2: ByVarParser[T2]): ByVarParser[T1 => T2] = _ => Fail
    }

  val syntax: BindingAlgebra[ByVarParser, Parser[String]] =
    new parser.BindingAlgebra.Syntax[Expr](testInterpreter)

  val varNameSyntax: Parser[String] = varName

  def grammar(self: BindingAlgebra.Expressions[ByVarParser, Type]): BindingAlgebra.Expressions[ByVarParser, Type] =
    new BindingAlgebra.Grammar[ByVarParser, Type, Parser[String]](
      self,
      syntax,
      varNameSyntax,
      byVarParserMonoidK,
      List(DoubleType)
    )

  val byVarParserDefer: Defer[ByVarParser] = new Defer[ByVarParser] {
    override def defer[A](fa: => ByVarParser[A]): ByVarParser[A] = vars => P(fa(vars))
  }

  val expressions: BindingAlgebra.Expressions[ByVarParser, Type] =
    BindingAlgebra.fixMultipleExpressions[ByVarParser, Type](
      byVarParserMonoidK,
      byVarParserDefer,
      List(_ => baseExpressions, grammar)
    )

  private def unsafeParseDouble(expression: String): Expr[Double] =
    expressions.value(DoubleType)(Nil).parse(expression).get.value

  private def unsafeParseLambda(expression: String): Expr[Double => Double] =
    expressions.func(DoubleType, expressions.value(DoubleType))(Nil).parse(expression).get.value

  private def unsafeParseHOLambda(expression: String): Expr[Double => Double => Double] =
    expressions
      .func(DoubleType, expressions.func(DoubleType, expressions.value(DoubleType)))(Nil)
      .parse(expression)
      .get
      .value

  private lazy val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
}
