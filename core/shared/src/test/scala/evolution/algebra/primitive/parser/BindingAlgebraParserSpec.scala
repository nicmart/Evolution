package evolution.algebra.primitive.parser

import evolution.data.HasValue
import evolution.drawing.algebra.interpreter.CtxString
import evolution.primitive.algebra.BindingAlgebra
import evolution.primitive.algebra.interpreter.{BindingAlgebraSerializer, DebugAlgebraSerializer}
import evolution.primitive.algebra.parser.{BindingAlgebraParser, ExtensibleParser, ParserConfig, ParsersContainerOps}
import ExtensibleParser._
import org.scalatest.{Matchers, WordSpec}

class BindingAlgebraParserSpec extends WordSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import fastparse.noApi._
  import evolution.primitive.algebra.parser.PrimitiveParsers._

  "A Binding Algebra Parser" should {
    "parse a simple let expression" in {
      val serializedExpression = "let(x, 10.0)($x)"
      unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
    }

    "parse a nested let expression" in {
      val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, 2.0)($x))"
      unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
    }

    "parse a double nested let expression" in {
      val serializedExpression = "let(x, let(y, 1.0)($y))(let(z, let(u, $x)($u))($x))"
      def expectedExpression[F[_]](alg: BindingAlgebra[F], double: Double => F[Double]): F[Double] = {
        import alg._
        let("x", let("y", double(1.0))(var0))(let("z", let("u", shift(var0))(var0))(var0))
      }
      unsafeParse(serializedExpression, container.parser[CtxString[Double]]) shouldBe serializedExpression
    }
  }

  private def unsafeParse[T](expression: String, parser: Parser[CtxString[T]]): String =
    parser.parse(expression).get.value(Nil)

  private lazy val bindingParser = new BindingAlgebraParser[CtxString](BindingAlgebraSerializer)
  private lazy val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
  private lazy val doubleExtensibleParser: ExtensibleParser[Container, CtxString[Double]] =
    ExtensibleParser(doubleParser, _ => Fail)

  def extensibleAddition: ExtensibleParser[Container, CtxString[Double]] =
    extensibleBinaryOpParser[CtxString[Double]]("add", (a, b) => ctx => s"add(${a(ctx)}, ${b(ctx)})")
      .contramap[Container](_.parser[CtxString[Double]])

  case class Container(double: ExtensibleParser[Container, CtxString[Double]])
  object Container {
    val start = Container(doubleExtensibleParser)
    implicit val hasDouble: HasParser[Container, CtxString[Double]] =
      HasValue.instance(_.double, (c, p) => c.copy(double = p))
    implicit def ops(c: Container): ParsersContainerOps[Container] = new ParsersContainerOps(c)
  }

  lazy val container: Container = bindingParser.buildContainer1[Container, Double](Container.start)
}
