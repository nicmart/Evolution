package evolution.algebra.primitive.parser

import evolution.drawing.algebra.interpreter.CtxString
import evolution.primitive.algebra.interpreter.BindingAlgebraSerializer
import evolution.primitive.algebra.parser.{BindingAlgebraParser, ParserConfig}
import fastparse.noApi
import fastparse.noApi.Parser
import org.scalatest.{Matchers, WordSpec}

class BindingAlgebraParserSpec extends WordSpec with Matchers {
  import ParserConfig.White._
  import fastparse.noApi._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  "A Binding algebra parser" should {
    "parse a simple let expression" in {
      val bindingParser = new BindingAlgebraParser[CtxString](BindingAlgebraSerializer)
      val serializedExpression = "let(x, 10.0, $x)"
      val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
      val letParser: Parser[CtxString[Double]] = bindingParser.let(doubleParser)
      unsafeParse(serializedExpression, letParser) shouldBe serializedExpression
    }

    "parse a nested let expression" in {
      val bindingParser = new BindingAlgebraParser[CtxString](BindingAlgebraSerializer)
      val serializedExpression = "let(x, let(y, 1.0, $y), let(z, 2.0, $x))"
      val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
      def letParser: Parser[CtxString[Double]] =
        bindingParser.let(doubleParser)
      unsafeParse(serializedExpression, letParser) shouldBe serializedExpression
    }

    "parse a double nested let expression" in {
      val bindingParser = new BindingAlgebraParser[CtxString](BindingAlgebraSerializer)
      // $x is not recognised
      val serializedExpression = "let(x, let(y, 1.0, $y), let(z, let(u, $x, $u), $x))"
      val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
      def letParser: Parser[CtxString[Double]] =
        bindingParser.let(doubleParser)
      unsafeParse(serializedExpression, letParser) shouldBe serializedExpression
    }
  }

  private def unsafeParse[T](expression: String, parser: Parser[CtxString[T]]): String =
    parser.parse(expression).get.value(Nil)
}
