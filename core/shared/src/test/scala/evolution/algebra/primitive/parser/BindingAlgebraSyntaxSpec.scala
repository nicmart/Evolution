package evolution.algebra.primitive.parser

import evolution.drawing.algebra.interpreter.CtxString
import evolution.primitive.algebra.BindingAlgebra
import evolution.primitive.algebra.interpreter.BindingAlgebraSerializer
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Matchers}

class BindingAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import fastparse.noApi._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are " - {
      "simple" in {
        pending
        val serializedExpression = "let(x, 10.0)($x)"
        val expected: Expr[Double] = Let("x", Value(10.0), Var0())
        unsafeParseDouble(serializedExpression) shouldBe expected
      }

      "nested" in {
        pending
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      "multi-nested" in {
        pending
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      // TODO HERE expose the lambda parser somehow
      "inside a lambda" in {
        pending
        val serializedExpression = "app(x -> let(y, 1.0)($x), 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }
    }

    // TODO HERE expose the lambda parser somehow
    "lambda expressions that are" - {
      pending
      "constant" in {
        val serializedExpression = "app(x -> 1.0, 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      "identity" in {
        val serializedExpression = "app(x -> $x, 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      "nested" in {
        val serializedExpression = "app(x -> app(y -> $x, 1.0), 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      "with multiple variables" in {
        val serializedExpression = "app(app(x -> y -> $x, 1.0), 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0)(app(y -> $x, 1.0))"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }
    }

    "fix expressions that are" - {
      pending
      "constant" in {
        val serializedExpression = "fix(self -> 1.0)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      "identities" in {
        val serializedExpression = "fix(self -> $self)"
        unsafeParseDouble(serializedExpression) shouldBe serializedExpression
      }

      // We need to make this work
      "fixed points of HOF" ignore {
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

  private def unsafeParseDouble(expression: String): Expr[Double] =
    ???

  private lazy val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
}
