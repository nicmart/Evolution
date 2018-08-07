package evolution.algebra.primitive.parser

import evolution.data.HasValue
import evolution.drawing.algebra.interpreter.CtxString
import evolution.primitive.algebra.BindingAlgebra
import evolution.primitive.algebra.interpreter.BindingAlgebraSerializer
import evolution.primitive.algebra.parser.DependentParser.HasParser
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Matchers, WordSpec}

class BindingAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import fastparse.noApi._

  "A Binding Algebra Parser should parse" - {
    "let expressions that are" - {
      "simple" in {
        val serializedExpression = "let(x, 10.0)($x)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      "nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      "multi-nested" in {
        val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
        def expectedExpression[F[_]](alg: BindingAlgebra[F], double: Double => F[Double]): F[Double] = {
          import alg._
          let("x", let("y", double(1.0))(var0))(let("z", let("u", shift(var0))(var0))(var0))
        }
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      // TODO HERE expose the lambda parser somehow
      "inside a lambda" in {
        val serializedExpression = "app(x -> let(y, 1.0)($x), 1.0)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }
    }

    // TODO HERE expose the lambda parser somehow
    "lambda expressions that are" - {
      "constant" in {
        val serializedExpression = "app(x -> 1.0, 1.0)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      "identity" in {
        val serializedExpression = "app(x -> $x, 1.0)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      "nested" in {
        val serializedExpression = "app(x -> app(y -> $x, 1.0), 1.0)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      "inside let expressions" in {
        val serializedExpression = "let(x, 1.0)(app(y -> $x, 1.0))"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }
    }

    "fix expressions that are" - {
      "constant" in {
        val serializedExpression = "fix(self -> 1.0)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      "identities" in {
        val serializedExpression = "fix(self -> $self)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }

      // We need to make this work
      "fixed points of HOF" ignore {
        val serializedExpression = "app( , 1.0)"
        unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
      }
    }
  }

  private def unsafeParse[T](expression: String, parser: Parser[CtxString[T]]): String =
    parser.parse(expression).get.value(Nil)

  private lazy val bindingParser = new BindingAlgebraParser[CtxString](BindingAlgebraSerializer)
  private lazy val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
  private lazy val dependentDoubleParser: DependentParser[Container, CtxString[Double]] =
    DependentParser(_ => doubleParser)

  def dependentAddition: DependentParser[Container, CtxString[Double]] =
    dependentBinaryOpParser[CtxString[Double]]("add", (a, b) => ctx => s"add(${a(ctx)}, ${b(ctx)})")
      .contramap[Container](_.parser[CtxString[Double]])

  case class Container(double: DependentParser[Container, CtxString[Double]], variables: List[String]) {
    def addVariable(name: String): Container = copy(variables = name :: variables)
  }
  object Container {
    val start = Container(dependentDoubleParser, Nil)
    implicit val hasDouble: HasParser[Container, CtxString[Double]] =
      HasValue.instance(_.double, (c, p) => c.copy(double = p))
    implicit def ops(c: Container): ParsersContainerOps[Container] = new ParsersContainerOps(c)
    implicit val hasVariables: HasVariables[Container] =
      HasVariables.instance[Container](_.variables, (name, c) => c.addVariable(name))
  }

  lazy val container: Container = bindingParser.buildContainer1[Container, Double](Container.start)
}
