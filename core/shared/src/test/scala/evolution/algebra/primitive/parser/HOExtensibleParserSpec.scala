package evolution.algebra.primitive.parser

import evolution.data.HasValue
import evolution.primitive.algebra.parser.{ExtParser, ExtensibleParser, ParserConfig}
import org.scalatest.{Matchers, WordSpec}

class HOExtensibleParserSpec extends WordSpec with Matchers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "An extensible parser" should {
    "parse an 'add' expression with a 'length' inside" in {
      val serializedExpression = """add(length("abc"), 2)"""
      val expectedExpression = Add(Length(Str("abc")), Num(2))
      val parser = HOExtensibleParser(
        extensibleAddition.extendWith(extensibleLengthParser),
        extensibleConcatenation.extendWith(extensibleConcatenation)
      )
      unsafeParse(serializedExpression, parser.doubleExpr) shouldBe expectedExpression
    }

    "parse a an expression with let bindings" in {
      val serializedExpression = """add(length("abc"), let(x, "a")(length($x))))"""
      val expectedExpression = Add(Length(Str("abc")), Let("x", Str("a"), Length(Var0())))
      unsafeParse(serializedExpression, fullExtensibleParser.doubleExpr) shouldBe expectedExpression
    }
  }

  def unsafeParse[T](expression: String, parser: Parser[T]): T =
    parser.parse(expression).get.value

  type HasString[T] = HasValue[T, Parser[Expression[String]]]
  type HasDouble[T] = HasValue[T, Parser[Expression[Double]]]

  case class HOExtensibleParser(
    double: ExtParser[HOExtensibleParser, Expression[Double]],
    string: ExtParser[HOExtensibleParser, Expression[String]]
  ) {
    def doubleExpr: Parser[Expression[Double]] = P(double.leaf | double.composite(this))
    def stringExpr: Parser[Expression[String]] = P(string.leaf | string.composite(this))
  }

  implicit class ParsersOps[C](container: C) {
    def parser[T, F[_]](implicit hasValue: HasValue[C, Parser[F[T]]]): Parser[F[T]] =
      hasValue.get(container)
    def withParser[T, F[_]](parser: Parser[F[T]])(implicit hasValue: HasValue[C, Parser[F[T]]]): C =
      hasValue.set(container, parser)
  }

  sealed trait Expression[T]
  case class Num(d: Double) extends Expression[Double]
  case class Str(s: String) extends Expression[String]
  case class Add(a: Expression[Double], b: Expression[Double]) extends Expression[Double]
  case class Concat(a: Expression[String], b: Expression[String]) extends Expression[String]
  case class Length(a: Expression[String]) extends Expression[Double]
  case class ToString(a: Expression[Double]) extends Expression[String]
  case class Let[A, B](name: String, value: Expression[A], body: Expression[B]) extends Expression[B]
  case class Var0[A]() extends Expression[A]
  case class VarS[A](expr: Expression[A]) extends Expression[A]

  def push[A](times: Int, expr: Expression[A]): Expression[A] =
    if (times <= 0) expr else VarS(push(times - 1, expr))
  def varN[A](n: Int): Expression[A] = push(n, Var0())

  lazy val doubleParser: Parser[Expression[Double]] = double.map(Num.apply)
  lazy val doubleExtensibleParser: ExtensibleParser[Expression[Double]] = ExtensibleParser(doubleParser)

  lazy val stringParser: Parser[Expression[String]] =
    P("\"" ~/ CharIn('a' to 'z').rep.! ~/ "\"").map(Str.apply)
  lazy val stringExtensibleParser = ExtensibleParser(stringParser)

  def additionParser(innerParser: HOExtensibleParser): Parser[Expression[Double]] =
    P(function2("add", innerParser.doubleExpr, innerParser.doubleExpr).map {
      case (expr1, expr2) =>
        Add(expr1, expr2)
    })

  def additionParser2[C: HasDouble](innerParsers: C): Parser[Expression[Double]] =
    P(function2("add", innerParsers.parser[Double, Expression], innerParsers.parser[Double, Expression]).map {
      case (expr1, expr2) =>
        Add(expr1, expr2)
    })

  def concatParser(innerParser: HOExtensibleParser): Parser[Expression[String]] =
    P(function2("concat", innerParser.stringExpr, innerParser.stringExpr).map {
      case (expr1, expr2) =>
        Concat(expr1, expr2)
    })

  def lengthParser(innerParser: HOExtensibleParser): Parser[Expression[Double]] =
    function1("length", innerParser.stringExpr).map(Length)

  def toStringParser(innerParser: HOExtensibleParser): Parser[Expression[String]] =
    function1("toString", innerParser.doubleExpr).map(ToString)

  def letSyntax[A, B](assignment: Parser[Expression[A]], body: String => Parser[Expression[B]]): Parser[Expression[B]] =
    functionFlatMap[(String, Expression[A]), Expression[B]](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => Let(name, valueExpr, bodyExpr))
    })

  lazy val extensibleAddition: ExtParser[HOExtensibleParser, Expression[Double]] =
    ExtParser(doubleParser, additionParser)
  lazy val extensibleConcatenation: ExtParser[HOExtensibleParser, Expression[String]] =
    ExtParser(stringParser, concatParser)
  lazy val extensibleLengthParser: ExtParser[HOExtensibleParser, Expression[Double]] =
    ExtParser(doubleParser, lengthParser)
  lazy val extensibleToStringParser: ExtParser[HOExtensibleParser, Expression[String]] =
    ExtParser(stringParser, toStringParser)

  lazy val extensibleLetBindingForNumToNum: ExtParser[HOExtensibleParser, Expression[Double]] =
    ExtParser(Fail, self => letSyntax(self.doubleExpr, name => addVarNameForNum(name, self).doubleExpr))
  lazy val extensibleLetBindingForNumToString: ExtParser[HOExtensibleParser, Expression[String]] =
    ExtParser(Fail, self => letSyntax(self.doubleExpr, name => addVarNameForNum(name, self).stringExpr))
  lazy val extensibleLetBindingForStringToNum: ExtParser[HOExtensibleParser, Expression[Double]] =
    ExtParser(Fail, self => letSyntax(self.stringExpr, name => addVarNameForString(name, self).doubleExpr))
  lazy val extensibleLetBindingForStringToString: ExtParser[HOExtensibleParser, Expression[String]] =
    ExtParser(Fail, self => letSyntax(self.stringExpr, name => addVarNameForString(name, self).stringExpr))

  lazy val fullExtensibleParser = HOExtensibleParser(
    extensibleAddition
      .extendWith(extensibleLengthParser)
      .extendWith(extensibleLetBindingForNumToNum)
      .extendWith(extensibleLetBindingForStringToNum),
    extensibleConcatenation
      .extendWith(extensibleToStringParser)
      .extendWith(extensibleLetBindingForNumToString)
      .extendWith(extensibleLetBindingForStringToString)
  )

  def var0[A](varName: String): Parser[Expression[A]] =
    varUsage(varName).map(_ => Var0())

  def addVarNameForNum[A](name: String, extensibleParser: HOExtensibleParser): HOExtensibleParser =
    HOExtensibleParser(
      extensibleParser.double.mapLeaf(leaf => leaf.map(expr => VarS(expr)) | var0(name)),
      extensibleParser.string
    )

  def addVarNameForString[A](name: String, extensibleParser: HOExtensibleParser): HOExtensibleParser =
    HOExtensibleParser(
      extensibleParser.double,
      extensibleParser.string.mapLeaf(leaf => leaf.map(expr => VarS(expr)) | var0(name))
    )
}
