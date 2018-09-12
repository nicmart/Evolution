package evolution.algebra.primitive.parser

import cats.{Defer, Id, MonoidK}
import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Inside, Matchers}
import fastparse.noApi._
import ParserConfig.White._
import fastparse.noApi

class CoreDrawingSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers with Inside {
  import Drawing._
  import ParserConfig.White._
  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParseEvolution(serializedExpression, DoubleType) shouldBe Empty[Double]()
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParseEvolution(serializedExpression, DoubleType) shouldBe Cons(DoubleScalar(1), Empty[Double]())
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParseEvolution(serializedExpression, DoubleType) shouldBe
          Cons(DoubleScalar(1), Cons(DoubleScalar(2), Cons(DoubleScalar(3), Empty[Double]())))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParseEvolution(serializedExpression, DoubleType) shouldBe
          MapEmpty(Cons(DoubleScalar(1), Empty[Double]()), Cons(DoubleScalar(2), Empty[Double]()))
      }

      "a mapCons expression" in {
        val serializedExpression = """mapCons(cons(1, empty), cons("abc", empty))"""
        val parsedExpression: MapCons[Double, String] =
          unsafeParseEvolution(serializedExpression, StringType).asInstanceOf[MapCons[Double, String]]
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

  object TestCoreDrawingAlgebraInterpreter extends CoreDrawingAlgebra[Scalar, Drawing, Id] {
    override def empty[A]: Drawing[A] = Empty()
    override def cons[A](head: Scalar[A], tail: Drawing[A]): Drawing[A] = Cons(head, tail)
    override def mapEmpty[A](eva: Drawing[A])(eva2: Drawing[A]): Drawing[A] = MapEmpty(eva, eva2)
    override def mapCons[A, B](eva: Drawing[A])(f: Scalar[A] => Drawing[A] => Drawing[B]): Drawing[B] = MapCons(eva, f)
  }

  // Interesting, this is analogous/dual of BasicExpressions
  // The type parameter moved from the method to the trait
  sealed trait TestType[T]
  object DoubleType extends TestType[Double]
  object StringType extends TestType[String]

  object BasicExpressions extends EmptyExpressions[Scalar, Drawing, Parser, TestType](parserMonoidK) {
    override def static[T](t: TestType[T]): Parser[Scalar[T]] = t match {
      case DoubleType => staticDouble
      case StringType => staticString
    }
    def staticDouble: Parser[Scalar[Double]] = double.map(d => DoubleScalar(d))
    def staticString: Parser[Scalar[String]] = stringLiteral.map(d => StringScalar(d))
  }

  type TestExpressions = Expressions[Scalar, Drawing, Parser, TestType]

  lazy val syntax: CoreDrawingAlgebra[Scalar, Drawing, Parser] =
    new CoreDrawingSyntax[Scalar, Drawing, Id](TestCoreDrawingAlgebraInterpreter)

  def expressions0(expressions: TestExpressions): TestExpressions =
    BasicExpressions

  def grammar(expressions: TestExpressions): TestExpressions =
    new Grammar[Scalar, Drawing, Parser, TestType](expressions, syntax, parserMonoidK, List(DoubleType, StringType))

  def mapConsExpression(expressions: TestExpressions): TestExpressions =
    new EmptyExpressions[Scalar, Drawing, Parser, TestType](parserMonoidK) {
      override def mapConsFunction[T1, T2](
        t1: TestType[T1],
        t2: TestType[T2]
      ): Parser[Scalar[T1] => Drawing[T1] => Drawing[T2]] =
        expressions.evolution(t2).map(evolution => _ => _ => evolution)
    }

  lazy val parserMonoidK: MonoidK[Parser] = new MonoidK[Parser] {
    override def empty[A]: noApi.Parser[A] = Fail
    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = P(x | y)
  }

  lazy val parserDefer: Defer[Parser] = new Defer[Parser] {
    override def defer[A](fa: => Parser[A]): Parser[A] =
      P(fa)
  }

  val combinedExpressions: TestExpressions =
    Expressions.fixMultipleExpressions[Scalar, Drawing, Parser, TestType](
      parserMonoidK,
      parserDefer,
      List(expressions0, mapConsExpression, grammar)
    )

  def unsafeParseEvolution[T](expression: String, t: TestType[T]): Drawing[T] =
    combinedExpressions.evolution(t).parse(expression).get.value
}
