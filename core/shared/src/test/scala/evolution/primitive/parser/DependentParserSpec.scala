package evolution.primitive.parser

import evolution.primitive.algebra.parser.{DependentParser, ParserConfig}
import fastparse.noApi
import org.scalatest.{Matchers, WordSpec}

class DependentParserSpec extends WordSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "A dependent parser" should {
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

  def unsafeParse(expression: String, dependentParser: DependentParser[Container, NumberExpression]): NumberExpression =
    unsafeParse(expression, dependentParser.parser(Container(dependentParser)))

  sealed trait NumberExpression
  case class Lit(d: Double) extends NumberExpression
  case class Add(a: NumberExpression, b: NumberExpression) extends NumberExpression
  case class Mul(a: NumberExpression, b: NumberExpression) extends NumberExpression

  case class Container(dependentParser: DependentParser[Container, NumberExpression]) {
    def expr: Parser[NumberExpression] = P(dependentParser.parser(this))
  }

  lazy val doubleExpr: DependentParser[Container, NumberExpression] =
    dependentDoubleParser[NumberExpression](Lit)
      .contramap[Container](_.expr)
  lazy val additionExpr: DependentParser[Container, NumberExpression] =
    dependentBinaryOpParser[NumberExpression]("add", Add)
      .contramap[Container](_.expr)
      .or(doubleExpr)
  lazy val multExpr: DependentParser[Container, NumberExpression] =
    dependentBinaryOpParser[NumberExpression]("mul", Mul)
      .contramap[Container](_.expr)
  lazy val addAndMultExpr: DependentParser[Container, NumberExpression] =
    additionExpr.or(multExpr)
}
