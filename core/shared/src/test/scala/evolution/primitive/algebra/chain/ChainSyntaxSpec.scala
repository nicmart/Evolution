package evolution.primitive.algebra.chain

import evolution.primitive.algebra.{ByVarParser, TestInterpreters}
import evolution.primitive.algebra.parser._
import cats.implicits._
import org.scalatest.{FreeSpec, Inside, Matchers}
import cats.kernel.Semigroup
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.evolution.parser.{EvolutionAlgebraExpressions, EvolutionAlgebraGrammar}

class ChainSyntaxSpec extends FreeSpec with Matchers with PrimitiveParsers with Inside with TestInterpreters {
  val interpreter: EvolutionAlgebra[Constant, ListExpr, Binding, String] = EvolutionAlgebraTestInterpreter
  import interpreter.bind._, interpreter.constants._, interpreter.list._, interpreter.list.{empty => nil}

  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParseEvolution(serializedExpression) shouldBe nil[Double]
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParseEvolution(serializedExpression) shouldBe cons(double(1), nil)
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParseEvolution(serializedExpression) shouldBe
          cons(double(1), cons(double(2), cons(double(3), nil)))
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParseEvolution(serializedExpression) shouldBe
          mapEmpty(cons(double(1), nil))(cons(double(2), nil))
      }

      "a mapCons expression" in {
        pending
        val serializedExpression = """mapCons(cons(1, empty), cons(1, empty))"""
        val expected = mapCons(cons(double(1), nil))(constantFunc(cons(double(1), nil)))
        unsafeParseEvolution(serializedExpression) shouldBe expected
      }
    }
  }

  def constantFunc[A, B](b: Binding[ListExpr[B]]): Binding[Constant[A] => ListExpr[A] => ListExpr[B]] =
    Lift(ConstantMapConsFunc[A, B](b))

  case class ConstantMapConsFunc[A, B](b: Binding[ListExpr[B]]) extends (Constant[A] => (ListExpr[A] => ListExpr[B])) {
    def apply(x: Constant[A]): ListExpr[A] => ListExpr[B] = ???
  }

  type BindingParser[T] = ByVarParser[Binding, T]

  def expressions: EvolutionAlgebraExpressions[Constant, ListExpr, ByVarParser[Binding, ?]] =
    EvolutionAlgebraGrammar.grammar(interpreter)

  def unsafeParseEvolution[T](expression: String): Binding[ListExpr[Double]] =
    expressions.list.evolutionOf(expressions.constants.doubles)(Semigroup[Double])(Nil).parse(expression).get.value
}
