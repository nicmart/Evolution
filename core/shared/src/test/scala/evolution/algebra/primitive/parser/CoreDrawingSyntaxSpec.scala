package evolution.algebra.primitive.parser

import cats.{Defer, Eval, Id, MonoidK}
import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ParsersContainerOps._
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

  object BasicExpressions extends EmptyExpressions[Scalar, Drawing, LazyParser, TestType](lazyParserMonoidK) {
    override def static[T](t: TestType[T]): LazyParser[Scalar[T]] = t match {
      case DoubleType => staticDouble
      case StringType => staticString
    }
    def staticDouble: LazyParser[Scalar[Double]] = Eval.later(double.map(d => DoubleScalar(d)))
    def staticString: LazyParser[Scalar[String]] = Eval.later(stringLiteral.map(d => StringScalar(d)))
  }

  type TestExpressions = Expressions[Scalar, Drawing, LazyParser, TestType]
  type LazyParser[T] = Eval[Parser[T]]

  lazy val syntax: CoreDrawingAlgebra[Scalar, Drawing, LazyParser] =
    new LazyCoreDrawingAlgebra[Scalar, Drawing, Parser](new CoreDrawingSyntax(TestCoreDrawingAlgebraInterpreter))

  def expressions0(expressions: Eval[TestExpressions]): TestExpressions =
    BasicExpressions

  def grammar(expressions: Eval[TestExpressions]): TestExpressions =
    new Grammar[Scalar, Drawing, LazyParser, TestType](
      expressions.value,
      syntax,
      lazyParserMonoidK,
      List(DoubleType, StringType)
    )

  def mapConsExpression(expressions: Eval[TestExpressions]): TestExpressions =
    new EmptyExpressions[Scalar, Drawing, LazyParser, TestType](lazyParserMonoidK) {
      override def mapConsFunction[T1, T2](
        t1: TestType[T1],
        t2: TestType[T2]
      ): LazyParser[Scalar[T1] => Drawing[T1] => Drawing[T2]] =
        Eval.later(expressions.value.evolution(t2).value.map(evolution => _ => _ => evolution))
    }

  lazy val parserMonoidK: MonoidK[Parser] = new MonoidK[Parser] {
    override def empty[A]: noApi.Parser[A] = Fail
    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = P(x | y)
  }

  lazy val lazyParserMonoidK: MonoidK[λ[α => Eval[Parser[α]]]] = new MonoidK[λ[α => Eval[Parser[α]]]] {
    override def empty[A]: Eval[Parser[A]] = Eval.now(Fail)
    override def combineK[A](x: Eval[Parser[A]], y: Eval[Parser[A]]): Eval[Parser[A]] =
      Eval.now(P(x.value | y.value))
  }

  lazy val lazyParserDefer: Defer[LazyParser] = new Defer[LazyParser] {
    override def defer[A](fa: => LazyParser[A]): LazyParser[A] =
      Eval.now(P(fa.value))
  }

  val combinedExpressions: TestExpressions =
    Expressions.fixMultipleExpressions[Scalar, Drawing, LazyParser, TestType](
      lazyParserMonoidK,
      lazyParserDefer,
      List(expressions0, mapConsExpression, grammar)
    )

  def unsafeParseEvolution[T](expression: String, t: TestType[T]): Drawing[T] =
    combinedExpressions.evolution(t).value.parse(expression).get.value
}
