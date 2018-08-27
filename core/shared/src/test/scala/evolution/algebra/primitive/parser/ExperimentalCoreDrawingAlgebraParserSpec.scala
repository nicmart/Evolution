package evolution.algebra.primitive.parser

import cats.Id
import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ParsersContainerOps._
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Inside, Matchers}
import fastparse.noApi._

class ExperimentalCoreDrawingAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers with Inside {
  import Drawing._
  import ParserConfig.White._
  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        //unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe Empty[Double]()
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
//        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe Cons(
//          DoubleScalar(1),
//          Empty[Double]()
//        )
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
//        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe
//          Cons(DoubleScalar(1), Cons(DoubleScalar(2), Cons(DoubleScalar(3), Empty[Double]())))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
//        unsafeParse(serializedExpression, container.parser[Drawing, Double]) shouldBe
//          MapEmpty(Cons(DoubleScalar(1), Empty[Double]()), Cons(DoubleScalar(2), Empty[Double]()))
      }

      "a mapCons expression" in {
        val serializedExpression = """mapCons(cons(1, empty), cons("abc", empty))"""
//        val parsedExpression: MapCons[Double, String] =
//          unsafeParse(serializedExpression, container.parser[Drawing, String]).asInstanceOf[MapCons[Double, String]]
//        parsedExpression.eva shouldBe Cons(DoubleScalar(1), Empty())
//        parsedExpression.f(DoubleScalar(1232132))(Empty[Double]()) shouldBe Cons(StringScalar("abc"), Empty())
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

  object TestCoreDrawingAlgebraInterpreter extends CoreDrawingAlgebra[Scalar, Drawing, Id] {
    override def empty[A]: Drawing[A] = Empty()
    override def cons[A](head: Scalar[A], tail: Drawing[A]): Drawing[A] = Cons(head, tail)
    override def mapEmpty[A](eva: Drawing[A])(eva2: Drawing[A]): Drawing[A] = MapEmpty(eva, eva2)
    override def mapCons[A, B](eva: Drawing[A])(f: Scalar[A] => Drawing[A] => Drawing[B]): Drawing[B] = MapCons(eva, f)
  }

  lazy val parserInterpreter: ExperimentalCoreDrawingAlgebraParser[Scalar, Drawing, Id] =
    new ExperimentalCoreDrawingAlgebraParser(TestCoreDrawingAlgebraInterpreter)

  lazy val grammarForDouble =
    new Grammar[Scalar, Drawing, Parser, Double](parserInterpreter, ExperimentalCoreDrawingAlgebraParser.monoidK[Id])
  lazy val grammarForString =
    new Grammar[Scalar, Drawing, Parser, String](parserInterpreter, ExperimentalCoreDrawingAlgebraParser.monoidK[Id])
}
