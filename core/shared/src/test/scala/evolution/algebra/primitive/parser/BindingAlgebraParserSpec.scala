//package evolution.algebra.primitive.parser
//
//import evolution.drawing.algebra.interpreter.CtxString
//import evolution.primitive.algebra.BindingAlgebra
//import evolution.primitive.algebra.interpreter.{BindingAlgebraSerializer, DebugAlgebraSerializer}
//import evolution.primitive.algebra.parser.{BindingAlgebraParser, ExtensibleParser, ParserConfig}
//import fastparse.{core, noApi}
//import fastparse.noApi.Parser
//import org.scalatest.{Matchers, WordSpec}
//
//class BindingAlgebraParserSpec extends WordSpec with Matchers {
//  import ParserConfig.White._
//  import fastparse.noApi._
//  import evolution.primitive.algebra.parser.PrimitiveParsers._
//
//  "A Binding Algebra Parser" should {
////    "parse a simple let expression" in new Fixture {
////      val serializedExpression = "let(x, 10.0, $x)"
////      unsafeParse(serializedExpression, letParser) shouldBe serializedExpression
////    }
////
//    "parse a nested let expression" in new Fixture {
//      val serializedExpression = "let(x, let(y, 1.0, $y), let(z, 2.0, $x))"
//      unsafeParse(serializedExpression, letParser) shouldBe serializedExpression
//    }
////
////    "parse a double nested let expression" in new Fixture {
////      val serializedExpression = "let(x, let(y, 1.0, $y), let(z, let(u, $x, $u), $x))"
////      def expectedExpression[F[_]](alg: BindingAlgebra[F], double: Double => F[Double]): F[Double] = {
////        import alg._
////        let("x", let("y", double(1.0))(var0))(let("z", let("u", shift(var0))(var0))(var0))
////      }
////
////      unsafeParse(serializedExpression, letParser) shouldBe serializedExpression
////    }
//
//    "parse an expression with addition" in new Fixture {
////      val serializedExpression1 = "add(1.0, let(y, 1.0, $y))"
////      unsafeParse(serializedExpression1, letParser) shouldBe serializedExpression1
//
////      val serializedExpression2 = "let(x, add(1.0, let(y, 1.0, $y)), add(1.0, 2.0))"
////      unsafeParse(serializedExpression2, letParser) shouldBe serializedExpression2
//
////      val serializedExpression3 = "let(x, 1.0, add($x, 2.0))"
////      unsafeParse(serializedExpression3, letParser) shouldBe serializedExpression3
//    }
//  }
//
//  private def unsafeParse[T](expression: String, parser: Parser[CtxString[T]]): String =
//    parser.parse(expression).get.value(Nil)
//
//  trait Fixture {
//    val bindingParser = new BindingAlgebraParser[CtxString](BindingAlgebraSerializer)
//    val debugParser = new BindingAlgebraParser[CtxString](DebugAlgebraSerializer)
//    val doubleParser: Parser[CtxString[Double]] = double.map(d => _ => d.toString)
//    def additionParser(innerParser: Parser[CtxString[Double]]): Parser[CtxString[Double]] =
//      P(function2("add", innerParser, innerParser).map {
//        case (expr1, expr2) =>
//          ctx =>
//            s"add(${expr1(ctx)}, ${expr2(ctx)})"
//      })
//    val letParser: Parser[CtxString[Double]] =
//      debugParser.decorateWithLet(additionParser, doubleParser)
//
//    val extensibleAdditionParser = ExtensibleParser(doubleParser, additionParser)
//
//  }
//}
