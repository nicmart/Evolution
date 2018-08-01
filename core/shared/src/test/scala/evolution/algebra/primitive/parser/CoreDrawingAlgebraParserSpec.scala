package evolution.algebra.primitive.parser

import evolution.data.HasValue
import evolution.geometry.Point
import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ExtensibleParser.HasParser
import evolution.primitive.algebra.parser.ParsersContainerOps._
import evolution.primitive.algebra.parser.{CoreDrawingAlgebraParser, ExtensibleParser, ParserConfig}
import org.scalatest.{FreeSpec, Matchers}

class CoreDrawingAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._
  import Drawing._
  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe Empty[Double]()
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe Cons(
          DoubleScalar(1),
          Empty[Double]()
        )
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe
          Cons(DoubleScalar(1), Cons(DoubleScalar(2), Cons(DoubleScalar(3), Empty[Double]())))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParse(serializedExpression, container.parser[Drawing[Double]]) shouldBe
          MapEmpty(Cons(DoubleScalar(1), Empty[Double]()), Cons(DoubleScalar(2), Empty[Double]()))
      }

      "a mapCons expression" in {
        val serializedExpression = """mapCons(cons(1, empty),cons("abc", empty))"""
        unsafeParse(serializedExpression, container.parser[Drawing[String]]) shouldBe
          MapCons(Cons(DoubleScalar(1), Empty()), Cons(StringScalar("abc"), Empty()))
      }
    }
  }

  sealed trait Scalar[A]
  case class DoubleScalar(d: Double) extends Scalar[Double]
  case class StringScalar(s: String) extends Scalar[String]

  sealed trait Drawing[A]
  object Drawing {
    case class Empty[A]() extends Drawing[A]
    case class Cons[A](head: Scalar[A], tail: Drawing[A]) extends Drawing[A]
    case class MapEmpty[A](eva: Drawing[A], eva2: Drawing[A]) extends Drawing[A]
    case class MapCons[A, B](eva: Drawing[A], f: Drawing[B]) extends Drawing[B]
  }

  object TestCoreDrawingAlgebraInterpreter extends CoreDrawingAlgebra[Scalar, Drawing] {
    override def empty[A]: Drawing[A] = Empty()
    override def cons[A](head: Scalar[A], tail: Drawing[A]): Drawing[A] = Cons(head, tail)
    override def mapEmpty[A](eva: Drawing[A])(eva2: Drawing[A]): Drawing[A] = MapEmpty(eva, eva2)
    override def mapCons[A, B](eva: Drawing[A])(f: Drawing[B]): Drawing[B] = MapCons(eva, f)
  }

  case class Container[S[_], F[_]](
    doubleParserS: ExtensibleParser[Container[S, F], S[Double]],
    doubleParserF: ExtensibleParser[Container[S, F], F[Double]],
    stringParserS: ExtensibleParser[Container[S, F], S[String]],
    stringParserF: ExtensibleParser[Container[S, F], F[String]]
  )
  object Container {
    implicit def hasDoubleFParser[S[_], F[_]]: HasParser[Container[S, F], F[Double]] =
      HasParser.instance[Container[S, F], F[Double]](
        _.doubleParserF,
        (c, p) => Container[S, F](c.doubleParserS, p, c.stringParserS, c.stringParserF)
      )
    implicit def hasDoubleSParser[S[_], F[_]]: HasParser[Container[S, F], S[Double]] =
      HasParser.instance[Container[S, F], S[Double]](
        _.doubleParserS,
        (c, p) => Container[S, F](p, c.doubleParserF, c.stringParserS, c.stringParserF)
      )
    implicit def hasStringFParser[S[_], F[_]]: HasParser[Container[S, F], F[String]] =
      HasParser.instance[Container[S, F], F[String]](
        _.stringParserF,
        (c, p) => Container[S, F](c.doubleParserS, c.doubleParserF, c.stringParserS, p)
      )
    implicit def hasStringSParser[S[_], F[_]]: HasParser[Container[S, F], S[String]] =
      HasParser.instance[Container[S, F], S[String]](
        _.stringParserS,
        (c, p) => Container[S, F](c.doubleParserS, c.doubleParserF, p, c.stringParserF)
      )
  }

  lazy val coreAlgebraParser = new CoreDrawingAlgebraParser(TestCoreDrawingAlgebraInterpreter)
  def doubleScalarParser[C]: ExtensibleParser[C, Scalar[Double]] =
    ExtensibleParser.Leaf(double.map(d => DoubleScalar(d)))
  def stringScalarParser[C]: ExtensibleParser[C, Scalar[String]] =
    ExtensibleParser.Leaf(stringLiteral.map(s => StringScalar(s)))

  lazy val literalContainer: Container[Scalar, Drawing] =
    Container(
      doubleScalarParser[Container[Scalar, Drawing]],
      ExtensibleParser.Empty(),
      stringScalarParser[Container[Scalar, Drawing]],
      ExtensibleParser.Empty()
    )

  lazy val container: Container[Scalar, Drawing] =
    coreAlgebraParser.buildContainer2[Container[Scalar, Drawing], Double, String](literalContainer)
}
