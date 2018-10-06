package evolution.primitive.algebra.list

import cats.{Defer, Id, MonoidK}
import evolution.primitive.algebra.{Composed, TestInterpreters}
import evolution.primitive.parser.CommonTestParsers
import evolution.primitive.algebra.list.parser.{Expressions, ListAlgebraGrammar, ListAlgebraSyntax, Type}
import evolution.primitive.algebra.parser._
import fastparse.noApi
import fastparse.noApi._
import org.scalatest.{FreeSpec, Inside, Matchers}
import ParserConfig.White._

class ListAlgebraSyntaxSpec extends FreeSpec with Matchers with CommonTestParsers with Inside with TestInterpreters {
  val interpreter: ListAlgebra[Constant, ListExpr, Binding] = ListAlgebraTestInterpreter
  import interpreter._, interpreter.{empty => nil}

  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe nil[Double]
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe cons(double(1), nil)
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe
          cons(double(1), cons(double(2), cons(double(3), nil)))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParseEvolution(serializedExpression, doubleType) shouldBe
          mapEmpty(cons(double(1), nil))(cons(double(2), nil))
      }

      "a mapCons expression" in {
        val serializedExpression = """mapCons(cons(1, empty), cons("abc", empty))"""
        val expected = mapCons(cons(double(1), nil))(constantFunc(cons(string("abc"), nil)))
        unsafeParseEvolution(serializedExpression, stringType) shouldBe expected
      }
    }
  }

  type TestExpressions = Expressions[Constant, ListExpr, Composed[Parser, Binding, ?]]
  type TestType[T] = Type[Constant, ListExpr, Composed[Parser, Binding, ?], T]

  def double(d: Double): Binding[Constant[Double]] = Lift(Value[Double](d))
  def string(s: String): Binding[Constant[String]] = Lift(Value[String](s))
  def constantFunc[A, B](b: Binding[ListExpr[B]]): Binding[Constant[A] => ListExpr[A] => ListExpr[B]] =
    Lift(ConstantMapConsFunc[A, B](b))

  case class ConstantMapConsFunc[A, B](b: Binding[ListExpr[B]]) extends (Constant[A] => (ListExpr[A] => ListExpr[B])) {
    def apply(x: Constant[A]): ListExpr[A] => ListExpr[B] = ???
  }

  val doubleType: TestType[Double] =
    Type[Constant, ListExpr, Composed[Parser, Binding, ?], Double](doubleLiteral.map(d => Lift(Value(d))), Fail)

  val stringType: TestType[String] =
    Type[Constant, ListExpr, Composed[Parser, Binding, ?], String](stringLiteral.map(d => Lift(Value(d))), Fail)

  lazy val syntax: ListAlgebra[Constant, ListExpr, Composed[Parser, Binding, ?]] =
    new ListAlgebraSyntax[Constant, ListExpr, Binding](ListAlgebraTestInterpreter)

  class BasicExpressions(self: TestExpressions) extends TestExpressions {
    override def static[T](t: TestType[T]): Parser[Binding[Constant[T]]] = t.static
    override def evolution[T](t: TestType[T]): Parser[Binding[ListExpr[T]]] = t.evolution
    override def mapConsFunction[T1, T2](
      t1: TestType[T1],
      t2: TestType[T2]
    ): Parser[Binding[Constant[T1] => ListExpr[T1] => ListExpr[T2]]] =
      self.evolution(t2).map(constantFunc)
  }

  def expressions0(expressions: TestExpressions): TestExpressions =
    new BasicExpressions(expressions)

  def grammar(expressions: TestExpressions): TestExpressions =
    new ListAlgebraGrammar[Constant, ListExpr, Composed[Parser, Binding, ?]](
      expressions,
      syntax,
      parserMonoidK,
      List(doubleType, stringType)
    )

  // TODO move somewhere
  lazy val parserMonoidK: MonoidK[Composed[Parser, Binding, ?]] = new MonoidK[Composed[Parser, Binding, ?]] {
    override def empty[A]: noApi.Parser[Binding[A]] = Fail
    override def combineK[A](x: Parser[Binding[A]], y: Parser[Binding[A]]): Parser[Binding[A]] = P(x | y)
  }

  // TODO move somewhere
  lazy val parserDefer: Defer[Composed[Parser, Binding, ?]] = new Defer[Composed[Parser, Binding, ?]] {
    override def defer[A](fa: => Parser[Binding[A]]): Parser[Binding[A]] =
      P(fa)
  }

  val combinedExpressions: TestExpressions =
    Expressions.fixMultipleExpressions[Constant, ListExpr, Composed[Parser, Binding, ?]](
      parserMonoidK,
      parserDefer,
      List(expressions0, grammar)
    )

  def unsafeParseEvolution[T](expression: String, t: TestType[T]): Binding[ListExpr[T]] =
    combinedExpressions.evolution(t).parse(expression).get.value
}
