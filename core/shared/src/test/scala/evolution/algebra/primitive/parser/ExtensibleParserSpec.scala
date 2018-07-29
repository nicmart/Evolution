package evolution.algebra.primitive.parser

import evolution.primitive.algebra.parser.{ExtensibleParser, ParserConfig}
import fastparse.noApi
import org.scalatest.{Matchers, WordSpec}

class ExtensibleParserSpec extends WordSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "An extensible parser" should {
    "parse non-composite expressions" in {
      val serializedExpression = "1"
      val expectedExpression = Lit(1)
      unsafeParse(serializedExpression, doubleExpr) shouldBe expectedExpression
    }

    "parse a composite expression" in {
      val serializedExpression = "add(1, 2)"
      val expectedExpression = Add(Lit(1), Lit(2))
      unsafeParse(serializedExpression, additionExpr) shouldBe expectedExpression
    }

    "parse a nested composite expression" in {
      val serializedExpression = "add(add(1, add(2, 3)), add(4, 5))"
      val expectedExpression = Add(Add(Lit(1), Add(Lit(2), Lit(3))), Add(Lit(4), Lit(5)))
      unsafeParse(serializedExpression, additionExpr) shouldBe expectedExpression
    }

    "parse a defferent and nested composite expressions" in {
      val serializedExpression = "add(mul(1, add(2, 3)), add(4, 5))"
      val expectedExpression = Add(Mul(Lit(1), Add(Lit(2), Lit(3))), Add(Lit(4), Lit(5)))
      unsafeParse(serializedExpression, addAndMultExpr) shouldBe expectedExpression
    }
  }

  def unsafeParse(
    expression: String,
    extensibleParser: ExtensibleParser[Container, NumberExpression]
  ): NumberExpression =
    unsafeParse(expression, extensibleParser.expr(Container(extensibleParser)))

  sealed trait NumberExpression
  case class Lit(d: Double) extends NumberExpression
  case class Add(a: NumberExpression, b: NumberExpression) extends NumberExpression
  case class Mul(a: NumberExpression, b: NumberExpression) extends NumberExpression

  case class Container(extensibleParser: ExtensibleParser[Container, NumberExpression]) {
    def expr: Parser[NumberExpression] = extensibleParser.expr(this)
  }

  lazy val doubleExpr: ExtensibleParser[Container, NumberExpression] =
    extensibleDoubleParser[NumberExpression](Lit)
      .contramap[Container](_.expr)
  lazy val additionExpr: ExtensibleParser[Container, NumberExpression] =
    extensibleBinaryOpParser[NumberExpression]("add", Add)
      .contramap[Container](_.expr)
      .extendWith(doubleExpr)
  lazy val multExpr: ExtensibleParser[Container, NumberExpression] =
    extensibleBinaryOpParser[NumberExpression]("mul", Mul)
      .contramap[Container](_.expr)
  lazy val addAndMultExpr: ExtensibleParser[Container, NumberExpression] =
    additionExpr.extendWith(multExpr)
}
