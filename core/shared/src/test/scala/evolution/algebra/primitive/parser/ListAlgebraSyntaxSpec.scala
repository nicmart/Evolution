package evolution.algebra.primitive.parser

import cats.{Defer, Id, MonoidK}
import evolution.primitive.algebra.parser._
import org.scalatest.{FreeSpec, Inside, Matchers}
import fastparse.noApi._
import ParserConfig.White._
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.list.parser.{Expressions, ListAlgebraGrammar, ListAlgebraSyntax, Type}
import fastparse.noApi

class ListAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers with Inside {
  import Drawing._
  import ParserConfig.White._
  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe Empty[Double]()
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe Cons(DoubleScalar(1), Empty[Double]())
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe
          Cons(DoubleScalar(1), Cons(DoubleScalar(2), Cons(DoubleScalar(3), Empty[Double]())))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe
          MapEmpty(Cons(DoubleScalar(1), Empty[Double]()), Cons(DoubleScalar(2), Empty[Double]()))
      }

      "a mapCons expression" in {
        val serializedExpression = """mapCons(cons(1, empty), cons("abc", empty))"""
        val parsedExpression: MapCons[Double, String] =
          unsafeParseEvolution(serializedExpression, stringType).asInstanceOf[MapCons[Double, String]]
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

  object TestListAlgebraInterpreter$ extends ListAlgebra[Scalar, Drawing, Id] {
    override def empty[A]: Drawing[A] = Empty()
    override def cons[A](head: Scalar[A], tail: Drawing[A]): Drawing[A] = Cons(head, tail)
    override def mapEmpty[A](eva: Drawing[A])(eva2: Drawing[A]): Drawing[A] = MapEmpty(eva, eva2)
    override def mapCons[A, B](eva: Drawing[A])(f: Scalar[A] => Drawing[A] => Drawing[B]): Drawing[B] = MapCons(eva, f)
  }

  type TestExpressions = Expressions[Scalar, Drawing, Parser]
  type TestType[T] = Type[Scalar, Drawing, Parser, T]

  val doubleType: TestType[Double] =
    Type[Scalar, Drawing, Parser, Double](double.map(d => DoubleScalar(d)), Fail)

  val stringType: TestType[String] =
    Type[Scalar, Drawing, Parser, String](stringLiteral.map(d => StringScalar(d)), Fail)

  lazy val syntax: ListAlgebra[Scalar, Drawing, Parser] =
    new ListAlgebraSyntax[Scalar, Drawing, Id](TestListAlgebraInterpreter$)

  class BasicExpressions(self: TestExpressions) extends TestExpressions {
    override def static[T](t: TestType[T]): Parser[Scalar[T]] = t.static
    override def evolution[T](t: TestType[T]): Parser[Drawing[T]] = t.evolution
    override def mapConsFunction[T1, T2](
      t1: TestType[T1],
      t2: TestType[T2]
    ): Parser[Scalar[T1] => Drawing[T1] => Drawing[T2]] =
      self.evolution(t2).map(evolution => _ => _ => evolution)
  }

  def expressions0(expressions: TestExpressions): TestExpressions =
    new BasicExpressions(expressions)

  def grammar(expressions: TestExpressions): TestExpressions =
    new ListAlgebraGrammar[Scalar, Drawing, Parser](expressions, syntax, parserMonoidK, List(doubleType, stringType))

  // TODO move somewhere
  lazy val parserMonoidK: MonoidK[Parser] = new MonoidK[Parser] {
    override def empty[A]: noApi.Parser[A] = Fail
    override def combineK[A](x: Parser[A], y: Parser[A]): Parser[A] = P(x | y)
  }

  // TODO move somewhere
  lazy val parserDefer: Defer[Parser] = new Defer[Parser] {
    override def defer[A](fa: => Parser[A]): Parser[A] =
      P(fa)
  }

  val combinedExpressions: TestExpressions =
    Expressions.fixMultipleExpressions[Scalar, Drawing, Parser](parserMonoidK, parserDefer, List(expressions0, grammar))

  def unsafeParseEvolution[T](expression: String, t: TestType[T]): Drawing[T] =
    combinedExpressions.evolution(t).parse(expression).get.value
}
