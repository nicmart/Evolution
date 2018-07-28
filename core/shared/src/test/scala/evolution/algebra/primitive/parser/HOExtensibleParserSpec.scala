package evolution.algebra.primitive.parser

import evolution.data.HasValue
import evolution.primitive.algebra.parser.{ExtensibleParser, ParserConfig, ParsersContainerOps}
import org.scalatest.{FreeSpec, Matchers, WordSpec}

class HOExtensibleParserSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._

  "An extensible parser" - {
    "parse an 'add' expression with a 'length' inside" in {
      val serializedExpression = """add(length("abc"), 2)"""
      val expectedExpression = Add(Length(Str("abc")), Num(2))
      val parser = emptyParser
        .withExtensibleParser[Double, Expression](
          doubleExpr
            .extendWith(additionExpr[HOExtensibleParser])
            .extendWith(lengthExpr)
        )
        .withExtensibleParser[String, Expression](
          stringExpr
            .extendWith(concatenationExpr[HOExtensibleParser])
            .extendWith(concatenationExpr)
        )
      unsafeParse(serializedExpression, parser.parser[Double, Expression]) shouldBe expectedExpression
    }

    "parse a an expression with let bindings" - {
      "string variable and numeric result" in {
        val serializedExpression = """add(length("abc"), let(x, "a")(length($x))))"""
        val expectedExpression = Add(Length(Str("abc")), Let("x", Str("a"), Length(Var0())))
        unsafeParse(serializedExpression, fullExtensibleParser(emptyParser).parser[Double, Expression]) shouldBe expectedExpression
      }
      "numeric variable and numeric result" in {
        val serializedExpression = """add(length("abc"), let(x, 12)(13)))"""
        val expectedExpression = Add(Length(Str("abc")), Let("x", Num(12), VarS(Num(13))))
        unsafeParse(serializedExpression, fullExtensibleParser(emptyParser).parser[Double, Expression]) shouldBe expectedExpression
      }
      "string variable and string result" in {
        val serializedExpression = """concat("abc", let(x, "a")("ah"))"""
        val expectedExpression = Concat(Str("abc"), Let("x", Str("a"), VarS(Str("ah"))))
        unsafeParse(serializedExpression, fullExtensibleParser(emptyParser).parser[String, Expression]) shouldBe expectedExpression
      }
      "numeric variable and string result" in {
        val serializedExpression = """concat("abc", let(x, 12)(toString($x)))"""
        val expectedExpression = Concat(Str("abc"), Let("x", Num(12), ToString(Var0())))
        unsafeParse(serializedExpression, fullExtensibleParser(emptyParser).parser[String, Expression]) shouldBe expectedExpression
      }
    }
  }

  def unsafeParse[T](expression: String, parser: Parser[T]): T =
    parser.parse(expression).get.value

  type Has[T, A] = HasValue[T, ExtensibleParser[T, Expression[A]]]
  type HasString[T] = HasValue[T, ExtensibleParser[T, Expression[String]]]
  type HasDouble[T] = HasValue[T, ExtensibleParser[T, Expression[Double]]]

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

  def doubleExpr[C]: ExtensibleParser[C, Expression[Double]] =
    ExtensibleParser(doubleParser, _ => Fail)
  def stringExpr[C]: ExtensibleParser[C, Expression[String]] =
    ExtensibleParser(stringParser, _ => Fail)
  def additionExpr[C: HasDouble]: ExtensibleParser[C, Expression[Double]] =
    extensibleBinaryOpParser[Expression[Double]]("add", Add)
      .contramap[C](c => c.parser[Double, Expression])
  def concatenationExpr[C: HasString]: ExtensibleParser[C, Expression[String]] =
    extensibleBinaryOpParser[Expression[String]]("concat", Concat)
      .contramap[C](c => c.parser[String, Expression])
  def lengthExpr[C: HasString]: ExtensibleParser[C, Expression[Double]] =
    extensibleUnaryOpParser[Expression[String], Expression[Double]]("length", Length)
      .contramap[C](c => c.parser[String, Expression])
  def toStringExpr[C: HasDouble]: ExtensibleParser[C, Expression[String]] =
    extensibleUnaryOpParser[Expression[Double], Expression[String]]("toString", ToString)
      .contramap[C](c => c.parser[Double, Expression])

  def extensibleLetBindingFor[C, VarType, ResultType](
    implicit
    hasVar: Has[C, VarType],
    hasRes: Has[C, ResultType]
  ): ExtensibleParser[C, Expression[ResultType]] =
    ExtensibleParser(
      Fail,
      self =>
        letParser[Expression[VarType], Expression[ResultType]](
          self.parser[VarType, Expression],
          name => addVarNameFor[VarType, C](name, self).parser[ResultType, Expression],
          Let.apply
      )
    )

  case class HOExtensibleParser(
    double: ExtensibleParser[HOExtensibleParser, Expression[Double]],
    string: ExtensibleParser[HOExtensibleParser, Expression[String]]
  )

  implicit def parserOps[C](container: C): ParsersContainerOps[C] =
    new ParsersContainerOps(container)

  object HOExtensibleParser {
    implicit val hasString: HasValue[HOExtensibleParser, ExtensibleParser[HOExtensibleParser, Expression[String]]] =
      HasValue.instance(_.string, (x, v) => x.copy(string = v))

    implicit val hasDouble: HasValue[HOExtensibleParser, ExtensibleParser[HOExtensibleParser, Expression[Double]]] =
      HasValue.instance(_.double, (x, v) => x.copy(double = v))
  }

  val emptyParser = HOExtensibleParser(ExtensibleParser.fail, ExtensibleParser.fail)

  def fullExtensibleParser[C: HasDouble: HasString](container: C): C =
    container
      .withExtensibleParser[Double, Expression](
        doubleExpr
          .extendWith(additionExpr)
          .extendWith(lengthExpr)
          .extendWith(extensibleLetBindingFor[C, Double, Double])
          .extendWith(extensibleLetBindingFor[C, String, Double])
      )
      .withExtensibleParser[String, Expression] {
        stringExpr
          .extendWith(concatenationExpr)
          .extendWith(toStringExpr)
          .extendWith(extensibleLetBindingFor[C, Double, String])
          .extendWith(extensibleLetBindingFor[C, String, String])
      }

  def var0[A](varName: String): Parser[Expression[A]] =
    varUsage(varName).map(_ => Var0())

  def addVarNameFor[T, C](name: String, container: C)(implicit has: Has[C, T]): C =
    container.withExtensibleParser(container.extensibleParser[T, Expression].transformLeaf { leaf =>
      leaf.map(expr => VarS(expr)) | var0(name)
    })
}
