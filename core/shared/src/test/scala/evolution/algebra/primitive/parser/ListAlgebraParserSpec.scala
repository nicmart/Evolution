package evolution.algebra.primitive.parser

import cats.Id
import evolution.data.HasValue
import evolution.geometry.Point
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.parser.ParsersContainerOps._
import evolution.primitive.algebra.parser.{CoreDrawingAlgebraParser, DependentParser, HasParser, ParserConfig}
import org.scalatest.{FreeSpec, Inside, Matchers}

class ListAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers with Inside {
  import ParserConfig.White._
  import evolution.primitive.algebra.parser.PrimitiveParsers._
  import fastparse.noApi._
  import Drawing._
  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe Empty[Double]()
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe Cons(
          DoubleScalar(1),
          Empty[Double]()
        )
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe
          Cons(DoubleScalar(1), Cons(DoubleScalar(2), Cons(DoubleScalar(3), Empty[Double]())))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe
          MapEmpty(Cons(DoubleScalar(1), Empty[Double]()), Cons(DoubleScalar(2), Empty[Double]()))
      }

      "a mapCons expression" in {
        val serializedExpression = """mapCons(cons(1, empty), cons("abc", empty))"""
        val parsedExpression: MapCons[Double, String] =
          unsafeParse(serializedExpression, container.parser[Drawing, String]).asInstanceOf[MapCons[Double, String]]
        parsedExpression.eva shouldBe Cons(DoubleScalar(1), Empty())
        parsedExpression.f(DoubleScalar(1232132))(Empty[Double]()) shouldBe Cons(StringScalar("abc"), Empty())
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
    case class MapCons[A, B](eva: Drawing[A], f: Scalar[A] => Drawing[A] => Drawing[B]) extends Drawing[B]
  }

  object TestCoreDrawingAlgebraInterpreter extends ListAlgebra[Scalar, Drawing, Id] {
    override def empty[A]: Drawing[A] = Empty()
    override def cons[A](head: Scalar[A], tail: Drawing[A]): Drawing[A] = Cons(head, tail)
    override def mapEmpty[A](eva: Drawing[A])(eva2: Drawing[A]): Drawing[A] = MapEmpty(eva, eva2)
    override def mapCons[A, B](eva: Drawing[A])(f: Scalar[A] => Drawing[A] => Drawing[B]): Drawing[B] = MapCons(eva, f)
  }

  case class Container[S[_], F[_]](
    doubleParserS: DependentParser[Container[S, F], S[Double]],
    doubleParserF: DependentParser[Container[S, F], F[Double]],
    stringParserS: DependentParser[Container[S, F], S[String]],
    stringParserF: DependentParser[Container[S, F], F[String]]
  )
  object Container {
    implicit def hasDoubleFParser[S[_], F[_]]: HasParser[Container[S, F], F, Double] =
      HasParser.instance[Container[S, F], F, Double](
        _.doubleParserF,
        (c, p) => Container[S, F](c.doubleParserS, p, c.stringParserS, c.stringParserF)
      )
    implicit def hasDoubleSParser[S[_], F[_]]: HasParser[Container[S, F], S, Double] =
      HasParser.instance[Container[S, F], S, Double](
        _.doubleParserS,
        (c, p) => Container[S, F](p, c.doubleParserF, c.stringParserS, c.stringParserF)
      )
    implicit def hasStringFParser[S[_], F[_]]: HasParser[Container[S, F], F, String] =
      HasParser.instance[Container[S, F], F, String](
        _.stringParserF,
        (c, p) => Container[S, F](c.doubleParserS, c.doubleParserF, c.stringParserS, p)
      )
    implicit def hasStringSParser[S[_], F[_]]: HasParser[Container[S, F], S, String] =
      HasParser.instance[Container[S, F], S, String](
        _.stringParserS,
        (c, p) => Container[S, F](c.doubleParserS, c.doubleParserF, p, c.stringParserF)
      )

    implicit def hasFunctionParser[S[_], F[_], T1, T2](
      implicit hasFT2: HasParser[Container[S, F], F, T2]
    ): HasParser[Container[S, F], Id, S[T1] => F[T1] => F[T2]] =
      HasParser.instance[Container[S, F], Id, S[T1] => F[T1] => F[T2]](
        c => hasFT2.get(c).map[S[T1] => F[T1] => F[T2]](ft2 => _ => _ => ft2),
        (a, b) => ???
      )
  }

  lazy val coreAlgebraParser = new CoreDrawingAlgebraParser(TestCoreDrawingAlgebraInterpreter)
  def doubleScalarParser[C]: DependentParser[C, Scalar[Double]] =
    DependentParser(_ => double.map(d => DoubleScalar(d)))
  def stringScalarParser[C]: DependentParser[C, Scalar[String]] =
    DependentParser(_ => stringLiteral.map(s => StringScalar(s)))

  lazy val literalContainer: Container[Scalar, Drawing] =
    Container(
      doubleScalarParser[Container[Scalar, Drawing]],
      DependentParser.empty,
      stringScalarParser[Container[Scalar, Drawing]],
      DependentParser.empty
    )

  lazy val container: Container[Scalar, Drawing] = {
    import Container._
    coreAlgebraParser.buildContainer2[Container[Scalar, Drawing], Double, String](literalContainer)
  }
}
