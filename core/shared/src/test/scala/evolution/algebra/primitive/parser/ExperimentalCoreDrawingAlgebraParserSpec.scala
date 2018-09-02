package evolution.algebra.primitive.parser

import cats.{Eval, Id, MonoidK}
import evolution.primitive.algebra.CoreDrawingAlgebra
import evolution.primitive.algebra.parser.ParsersContainerOps._
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Inside, Matchers}
import fastparse.noApi._
import ParserConfig.White._
import fastparse.noApi

class ExperimentalCoreDrawingAlgebraParserSpec extends FreeSpec with Matchers with CommonTestParsers with Inside {
  import Drawing._
  import ParserConfig.White._
  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParseEvolution(serializedExpression, DoubleType) shouldBe Empty[Double]()
      }

      "a cons expression" in {
        pending
        val serializedExpression = "cons(1, empty)"
        unsafeParseEvolution(serializedExpression, DoubleType) shouldBe Cons(DoubleScalar(1), Empty[Double]())
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

  sealed trait TestType[T] {
    def extractStatic(expressions: TestExpressions): Parser[Scalar[T]]
    def extractEvolution(expressions: TestExpressions): Parser[Drawing[T]]
    def extractMapConsFunction[S](
      expressions: TestExpressions,
      from: TestType[S]
    ): Parser[Scalar[S] => Drawing[S] => Drawing[T]] =
      extractEvolution(expressions).map(evolution => _ => _ => evolution)
  }
  object DoubleType extends TestType[Double] {
    override def extractStatic(expressions: TestExpressions): Parser[Scalar[Double]] = expressions.staticDouble
    override def extractEvolution(expressions: TestExpressions): Parser[Drawing[Double]] = expressions.evolutionDouble
  }
  object StringType extends TestType[String] {
    override def extractStatic(expressions: TestExpressions): Parser[Scalar[String]] = expressions.staticString
    override def extractEvolution(expressions: TestExpressions): Parser[Drawing[String]] = expressions.evolutionString
  }

  case class TestExpressions(
    staticDouble: Parser[Scalar[Double]],
    staticString: Parser[Scalar[String]],
    evolutionDouble: Parser[Drawing[Double]],
    evolutionString: Parser[Drawing[String]]
  ) extends Expressions[Scalar, Drawing, Parser, TestType] {
    override def static[T](t: TestType[T]): Parser[Scalar[T]] = t.extractStatic(this)
    override def evolution[T](t: TestType[T]): Parser[Drawing[T]] = t.extractEvolution(this)
    override def mapConsFunction[T1, T2](
      t1: TestType[T1],
      t2: TestType[T2]
    ): Parser[Scalar[T1] => Drawing[T1] => Drawing[T2]] = t2.extractMapConsFunction(this, t1)
  }

  val expressions0 =
    TestExpressions(double.map(d => DoubleScalar(d)), stringLiteral.map(d => StringScalar(d)), Fail, Fail)

  val orMonoid: MonoidK[Lambda[T => Eval[Parser[T]]]] = new MonoidK[Lambda[T => Eval[Parser[T]]]] {
    override def empty[A]: Eval[Parser[A]] = Eval.now(Fail)
    override def combineK[A](x: Eval[Parser[A]], y: Eval[Parser[A]]): Eval[Parser[A]] =
      Eval.now(P(x.value | y.value))
  }

  val grammar = new Grammar[Scalar, Drawing, Parser, TestType](
    new CoreDrawingSyntax(TestCoreDrawingAlgebraInterpreter),
    expressions0,
    orMonoid,
    List(DoubleType, StringType)
  )

  def unsafeParseEvolution[T](expression: String, t: TestType[T]): Drawing[T] =
    grammar.evolution(t).parse(expression).get.value
}
