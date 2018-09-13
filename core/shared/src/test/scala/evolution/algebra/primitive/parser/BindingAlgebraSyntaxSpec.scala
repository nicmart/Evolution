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
    pending
    "let expressions that are " - {
      "simple" in {
        val serializedExpression = "let(x, 10.0)($x)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      // TODO HERE expose the lambda parser somehow
      "inside a lambda" in {
        val serializedExpression = "app(x -> let(y, 1.0)($x), 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }
    }

    // TODO HERE expose the lambda parser somehow
    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "app(x -> 1.0, 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "identity" in {
        val serializedExpression = "app(x -> $x, 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "nested" in {
        val serializedExpression = "app(x -> app(y -> $x, 1.0), 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "with multiple variables" in {
        val serializedExpression = "app(app(x -> y -> $x, 1.0), 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0)(app(y -> $x, 1.0))"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(self -> 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      "identities" in {
        val serializedExpression = "fix(self -> $self)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }

      // We need to make this work
      "fixed points of HOF" ignore {
        val serializedExpression = "app( , 1.0)"
        unsafeParse(serializedExpression) shouldBe serializedExpression
      }
    }
  }

  private def unsafeParse[T](expression: String): String =
    ???

  private lazy val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
}
