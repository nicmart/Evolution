package evolution.primitive.algebra.chain.parser

import cats.implicits._
import cats.kernel.Semigroup
import evolution.primitive.algebra.TestInterpreters
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.parser.{ EvolutionExpressions, EvolutionGrammar }
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import evolution.primitive.algebra.parser._
import org.scalatest.{ FreeSpec, Inside, Matchers }

class ChainParserSyntaxSpec extends FreeSpec with Matchers with PrimitiveParsers with Inside with TestInterpreters {
  val interpreter: Evolution[ListExpr, Binding, Double, String, String] = EvolutionAlgebraTestInterpreter
  import interpreter.chain.{ empty => nil, _ }
  import interpreter.constants._

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
          mapEmpty(cons(double(1), nil), cons(double(2), nil))
      }

      "a mapCons expression" in {
        pending
        val serializedExpression = """mapCons(cons(1, empty), cons(1, empty))"""
        val expected = mapCons(cons(double(1), nil))(constantFunc(cons(double(1), nil)))
        unsafeParseEvolution(serializedExpression) shouldBe expected
      }
    }
  }

  def constantFunc[A, B](b: Binding[ListExpr[B]]): Binding[A => ListExpr[A] => ListExpr[B]] =
    Lift(ConstantMapConsFunc[A, B](b))

  case class ConstantMapConsFunc[A, B](b: Binding[ListExpr[B]]) extends (A => (ListExpr[A] => ListExpr[B])) {
    def apply(x: A): ListExpr[A] => ListExpr[B] = ???
  }

  type BindingParser[T] = ByVarParserK[Binding, T]

  def expressions: EvolutionExpressions[ListExpr, ByVarParserK[Binding, ?]] =
    EvolutionGrammar.grammar(interpreter)

  def unsafeParseEvolution[T](expression: String): Binding[ListExpr[Double]] =
    expressions.chain
      .evolutionOf(expressions.constants.doubles)(Semigroup[Double])
      .parser(Nil)
      .parse(expression)
      .get
      .value
}
