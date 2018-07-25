package evolution.algebra.primitive.parser

import evolution.data.HasValue
import evolution.primitive.algebra.parser.{ExtensibleParser, ParserConfig}
import org.scalatest.{Matchers, WordSpec}

class HOExtensibleParserSpec extends WordSpec with Matchers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "An extensible parser" should {
    "parse an 'add' expression with a 'length' inside" in {
      val serializedExpression = """add(length("abc"), 2)"""
      val expectedExpression = Add(Length(Str("abc")), Num(2))
      val parser = emptyParser
        .withExtensibleParser[Double, Expression](
          extensibleAddition[HOExtensibleParser].extendWith(extensibleLengthParser)
        )
        .withExtensibleParser[String, Expression](
          extensibleConcatenation[HOExtensibleParser].extendWith(extensibleConcatenation)
        )
      unsafeParse(serializedExpression, parser.parser[Double, Expression]) shouldBe expectedExpression
    }

    "parse a an expression with let bindings" in {
      val serializedExpression = """add(length("abc"), let(x, "a")(length($x))))"""
      val expectedExpression = Add(Length(Str("abc")), Let("x", Str("a"), Length(Var0())))
      unsafeParse(serializedExpression, fullExtensibleParser(emptyParser).parser[Double, Expression]) shouldBe expectedExpression
    }
  }

  def unsafeParse[T](expression: String, parser: Parser[T]): T =
    parser.parse(expression).get.value

  type Has[T, A] = HasValue[T, ExtensibleParser[T, Expression[A]]]
  type HasString[T] = HasValue[T, ExtensibleParser[T, Expression[String]]]
  type HasDouble[T] = HasValue[T, ExtensibleParser[T, Expression[Double]]]

  implicit class ParsersOps[C](container: C) {
    def extensibleParser[T, F[_]](
      implicit hasValue: HasValue[C, ExtensibleParser[C, F[T]]]
    ): ExtensibleParser[C, F[T]] =
      hasValue.get(container)
    def withExtensibleParser[T, F[_]](
      parser: ExtensibleParser[C, F[T]]
    )(implicit hasValue: HasValue[C, ExtensibleParser[C, F[T]]]): C =
      hasValue.set(container, parser)
    def parser[T, F[_]](implicit hasValue: HasValue[C, ExtensibleParser[C, F[T]]]): Parser[F[T]] =
      hasValue.get(container).expr(container)
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
  lazy val stringParser: Parser[Expression[String]] =
    P("\"" ~/ CharIn('a' to 'z').rep.! ~/ "\"").map(Str.apply)

  def additionParser[C: HasDouble](innerParsers: C): Parser[Expression[Double]] =
    P(function2("add", innerParsers.parser[Double, Expression], innerParsers.parser[Double, Expression]).map {
      case (expr1, expr2) =>
        Add(expr1, expr2)
    })

  def concatParser[C: HasString](innerParsers: C): Parser[Expression[String]] =
    P(function2("concat", innerParsers.parser[String, Expression], innerParsers.parser[String, Expression]).map {
      case (expr1, expr2) =>
        Concat(expr1, expr2)
    })

  def lengthParser[C: HasString](innerParsers: C): Parser[Expression[Double]] =
    function1("length", innerParsers.parser[String, Expression]).map(Length)

  def toStringParser[C: HasDouble](innerParsers: C): Parser[Expression[String]] =
    function1("toString", innerParsers.parser[Double, Expression]).map(ToString)

  def letSyntax[A, B](assignment: Parser[Expression[A]], body: String => Parser[Expression[B]]): Parser[Expression[B]] =
    functionFlatMap[(String, Expression[A]), Expression[B]](function2("let", varName, assignment), {
      case (name, valueExpr) => body(name).map(bodyExpr => Let(name, valueExpr, bodyExpr))
    })

  def extensibleAddition[C: HasDouble]: ExtensibleParser[C, Expression[Double]] =
    ExtensibleParser(doubleParser, additionParser[C])
  def extensibleConcatenation[C: HasString]: ExtensibleParser[C, Expression[String]] =
    ExtensibleParser(stringParser, concatParser[C])
  def extensibleLengthParser[C: HasString]: ExtensibleParser[C, Expression[Double]] =
    ExtensibleParser(doubleParser, lengthParser[C])
  def extensibleToStringParser[C: HasDouble]: ExtensibleParser[C, Expression[String]] =
    ExtensibleParser(stringParser, toStringParser[C])

  def extensibleLetBindingFor[C, VarType, ResultType](
    implicit
    hasVar: Has[C, VarType],
    hasRes: Has[C, ResultType]
  ): ExtensibleParser[C, Expression[ResultType]] =
    ExtensibleParser(
      Fail,
      self =>
        letSyntax(
          self.parser[VarType, Expression],
          name => addVarNameFor[VarType, C](name, self).parser[ResultType, Expression]
      )
    )

  case class HOExtensibleParser(
    double: ExtensibleParser[HOExtensibleParser, Expression[Double]],
    string: ExtensibleParser[HOExtensibleParser, Expression[String]]
  )

  object HOExtensibleParser {
    implicit val hasDouble: HasValue[HOExtensibleParser, ExtensibleParser[HOExtensibleParser, Expression[Double]]] =
      new HasValue[HOExtensibleParser, ExtensibleParser[HOExtensibleParser, Expression[Double]]] {
        override def get(x: HOExtensibleParser): ExtensibleParser[HOExtensibleParser, Expression[Double]] = x.double
        override def set(
          x: HOExtensibleParser,
          v: ExtensibleParser[HOExtensibleParser, Expression[Double]]
        ): HOExtensibleParser =
          x.copy(double = v)
      }
    implicit val hasString: HasValue[HOExtensibleParser, ExtensibleParser[HOExtensibleParser, Expression[String]]] =
      new HasValue[HOExtensibleParser, ExtensibleParser[HOExtensibleParser, Expression[String]]] {
        override def get(x: HOExtensibleParser): ExtensibleParser[HOExtensibleParser, Expression[String]] = x.string
        override def set(
          x: HOExtensibleParser,
          v: ExtensibleParser[HOExtensibleParser, Expression[String]]
        ): HOExtensibleParser =
          x.copy(string = v)
      }
  }

  val emptyParser = HOExtensibleParser(ExtensibleParser.fail, ExtensibleParser.fail)

  def fullExtensibleParser[C: HasDouble: HasString](container: C) =
    container
      .withExtensibleParser[Double, Expression](
        extensibleAddition
          .extendWith(extensibleLengthParser)
          .extendWith(extensibleLetBindingFor[C, Double, Double])
          .extendWith(extensibleLetBindingFor[C, String, Double])
      )
      .withExtensibleParser[String, Expression] {
        extensibleConcatenation
          .extendWith(extensibleToStringParser)
          .extendWith(extensibleLetBindingFor[C, Double, String])
          .extendWith(extensibleLetBindingFor[C, String, String])
      }

  def var0[A](varName: String): Parser[Expression[A]] =
    varUsage(varName).map(_ => Var0())

  def addVarNameFor[T, C](name: String, container: C)(implicit has: Has[C, T]): C =
    container.withExtensibleParser(container.extensibleParser[T, Expression].mapLeaf { leaf =>
      leaf.map(expr => VarS(expr)) | var0(name)
    })
}
