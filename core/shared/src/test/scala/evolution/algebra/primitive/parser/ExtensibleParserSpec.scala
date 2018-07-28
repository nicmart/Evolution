package evolution.algebra.primitive.parser

import evolution.primitive.algebra.parser.{ExtensibleParser, ParserConfig}
import fastparse.noApi
import org.scalatest.{Matchers, WordSpec}

class ExtensibleParserSpec extends WordSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "An extensible parser" should {
    "parse a simple add expression" in {
      val serializedExpression = "add(1, 2)"
      val expectedExpression = Add(Lit(1), Lit(2))
      unsafeParse(serializedExpression, additionExpr) shouldBe expectedExpression
    }

    "parse a nested add expression" in {
      val serializedExpression = "add(add(1, add(2, 3)), add(4, 5))"
      val expectedExpression = Add(Add(Lit(1), Add(Lit(2), Lit(3))), Add(Lit(4), Lit(5)))
      unsafeParse(serializedExpression, additionExpr) shouldBe expectedExpression
    }

    "parse a combined add and mul expression" in {
      val serializedExpression = "add(mul(1, add(2, 3)), add(4, 5))"
      val expectedExpression = Add(Mul(Lit(1), Add(Lit(2), Lit(3))), Add(Lit(4), Lit(5)))
      unsafeParse(serializedExpression, addAndMultExpr) shouldBe expectedExpression
    }

    "parse an add expression with a single variable" in {
      val serializedExpression = "add(1, $x)"
      val expectedExpression = Add(VarS(Lit(1)), Var0)
      unsafeParse(serializedExpression, addVarName("x", additionExpr)) shouldBe expectedExpression
    }

    "parse an add expression with multiple variables" in {
      val serializedExpression = "add(1, add(add($x, $z), $y))"
      val expectedExpression = Add(push(3, Lit(1)), Add(Add(Var0, varN(2)), varN(1)))
      val extensibleParser = addVarName("x", addVarName("y", addVarName("z", additionExpr)))
      unsafeParse(serializedExpression, extensibleParser) shouldBe expectedExpression
    }

    "parse a 'double' expression with let bindings" in {
      val serializedExpression = "let(x, 1)($x)"
      val expectedExpression = Let("x", Lit(1), Var0)
      val extensibleParser = doubleExpr.extendWith(letExpr)
      unsafeParse(serializedExpression, extensibleParser) shouldBe expectedExpression
    }

    "parse a nested 'double' expression with let bindings" in {
      val serializedExpression = "let(x, let(y, 1)($y))(let(z, $x)($x))"
      val expectedExpression = Let("x", Let("y", Lit(1), varN(0)), Let("z", varN(0), varN(1)))
      val extensibleParser = doubleExpr.extendWith(letExpr)
      unsafeParse(serializedExpression, extensibleParser) shouldBe expectedExpression
    }

    "parse a full expression expression with let bindings" in {
      val serializedExpression = "add(1, let(x, mul(2, 3))(mul($x, let(y, 1)(add($x, $y)))))"
      val expectedExpression =
        Add(Lit(1), Let("x", Mul(Lit(2), Lit(3)), Mul(varN(0), Let("y", VarS(Lit(1)), Add(varN(1), varN(0))))))
      val extensibleParser = addAndMultExpr.extendWith(letExpr)
      unsafeParse(serializedExpression, extensibleParser) shouldBe expectedExpression
    }
  }

  def unsafeParse(
    expression: String,
    extensibleParser: ExtensibleParser[Container, NumberExpression]
  ): NumberExpression = {
    extensibleParser.expr(Container(extensibleParser)).parse(expression).get.value
  }

  sealed trait NumberExpression
  case class Lit(d: Double) extends NumberExpression
  case class Add(a: NumberExpression, b: NumberExpression) extends NumberExpression
  case class Mul(a: NumberExpression, b: NumberExpression) extends NumberExpression
  case class Let(name: String, value: NumberExpression, body: NumberExpression) extends NumberExpression
  case object Var0 extends NumberExpression
  case class VarS(expr: NumberExpression) extends NumberExpression

  def push(times: Int, expr: NumberExpression): NumberExpression =
    if (times <= 0) expr else VarS(push(times - 1, expr))
  def varN(n: Int): NumberExpression = push(n, Var0)

  case class Container(extensibleParser: ExtensibleParser[Container, NumberExpression]) {
    def expr: Parser[NumberExpression] = extensibleParser.expr(this)
  }

  lazy val doubleExpr =
    extensibleDoubleParser[NumberExpression](Lit)
      .contramap[Container](_.expr)
  lazy val additionExpr =
    extensibleBinaryOpParser[NumberExpression]("add", Add)
      .contramap[Container](_.expr)
      .extendWith(doubleExpr)
  lazy val multExpr =
    extensibleBinaryOpParser[NumberExpression]("mul", Mul)
      .contramap[Container](_.expr)
  lazy val addAndMultExpr: ExtensibleParser[Container, NumberExpression] =
    additionExpr.extendWith(multExpr)
  lazy val letExpr: ExtensibleParser[Container, NumberExpression] =
    ExtensibleParser(
      Fail,
      self => letParser(self.expr, name => addVarNameToContainer(name, self.extensibleParser).expr, Let.apply)
    )

  def var0(varName: String): Parser[NumberExpression] =
    varUsage(varName).map(_ => Var0)

  def addVarName(
    name: String,
    extensibleParser: ExtensibleParser[Container, NumberExpression]
  ): ExtensibleParser[Container, NumberExpression] =
    extensibleParser.transformLeaf(leaf => leaf.map(expr => VarS(expr)) | var0(name))

  def addVarNameToContainer(name: String, extensibleParser: ExtensibleParser[Container, NumberExpression]): Container =
    Container(addVarName(name, extensibleParser))
}
