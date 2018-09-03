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
  sealed trait TestType[T] {
    def extractStatic(expressions: BasicExpressions): Parser[Scalar[T]]
    def extractEvolution(expressions: BasicExpressions): Parser[Drawing[T]]
    // Clarify this bit
    def extractMapConsFunction[S](
      expressions: Expressions[Scalar, Drawing, Parser, TestType],
      from: TestType[S]
    ): Parser[Scalar[S] => Drawing[S] => Drawing[T]] =
      expressions.evolution(this).map(evolution => _ => _ => evolution)
  }

  object DoubleType extends TestType[Double] {
    override def extractStatic(expressions: BasicExpressions): Parser[Scalar[Double]] = expressions.staticDouble
    override def extractEvolution(expressions: BasicExpressions): Parser[Drawing[Double]] = expressions.evolutionDouble
  }
  object StringType extends TestType[String] {
    override def extractStatic(expressions: BasicExpressions): Parser[Scalar[String]] = expressions.staticString
    override def extractEvolution(expressions: BasicExpressions): Parser[Drawing[String]] = expressions.evolutionString
  }

  case class BasicExpressions(
    self: Expressions[Scalar, Drawing, Parser, TestType],
    staticDouble: Parser[Scalar[Double]],
    staticString: Parser[Scalar[String]],
    evolutionDouble: Parser[Drawing[Double]],
    evolutionString: Parser[Drawing[String]]
  ) extends Expressions[Scalar, Drawing, Parser, TestType] {
    override def static[T](t: TestType[T]): Parser[Scalar[T]] = t.extractStatic(this)
    override def evolution[T](t: TestType[T]): Parser[Drawing[T]] = t.extractEvolution(this)
    // The problem is here, inside mapCons we have "this", that does not contain "cons()" and the others
    override def mapConsFunction[T1, T2](
      t1: TestType[T1],
      t2: TestType[T2]
    ): Parser[Scalar[T1] => Drawing[T1] => Drawing[T2]] = t2.extractMapConsFunction(self, t1)
  }

  type TestExpressions = Expressions[Scalar, Drawing, Parser, TestType]

  def expressions0(expressions: TestExpressions): TestExpressions =
    BasicExpressions(expressions, double.map(d => DoubleScalar(d)), stringLiteral.map(d => StringScalar(d)), Fail, Fail)

  val orMonoid: MonoidK[Lambda[T => Eval[Parser[T]]]] = new MonoidK[Lambda[T => Eval[Parser[T]]]] {
    override def empty[A]: Eval[Parser[A]] = Eval.now(Fail)
    override def combineK[A](x: Eval[Parser[A]], y: Eval[Parser[A]]): Eval[Parser[A]] =
      Eval.now(P(x.value | y.value))
  }

  def grammar(expressions: TestExpressions): TestExpressions = new Grammar[Scalar, Drawing, Parser, TestType](
    new CoreDrawingSyntax(TestCoreDrawingAlgebraInterpreter),
    expressions,
    orMonoid,
    List(DoubleType, StringType)
  )

  val combinedExpressions: TestExpressions =
    Expressions.fixMultipleExpressions[Scalar, Drawing, Parser, TestType](orMonoid, List(expressions0, grammar))

  def unsafeParseEvolution[T](expression: String, t: TestType[T]): Drawing[T] =
    combinedExpressions.evolution(t).parse(expression).get.value
}
