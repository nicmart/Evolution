package evolution.primitive.algebra.chain.parser

import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.interpreter.Types.{ F, R }
import evolution.primitive.algebra.evolution.interpreter.{ EvolutionTypedSerializer, Types }
import evolution.primitive.algebra.evolution.parser.{ EvolutionGrammar, Expressions }
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi
import org.scalatest.{ FreeSpec, Inside, Matchers }

class ChainParserSyntaxSpec extends FreeSpec with Matchers with Inside {
  val interpreter: Evolution[F, R, String, String] = new EvolutionTypedSerializer
  import interpreter.chain.{ empty => nil, _ }
  import interpreter.constants._

  "A CoreDrawingAlgebraParser" - {
    "should parse" - {
      "an empty expression" in {
        val serializedExpression = "empty"
        unsafeParseEvolution(serializedExpression) shouldBe nil[Double].infer(Types.evolutionOfDoubles).toString
      }

      "a cons expression" in {
        val serializedExpression = "cons(1, empty)"
        unsafeParseEvolution(serializedExpression) shouldBe cons[Double](double(1), nil)
          .infer(Types.evolutionOfDoubles)
          .toString
      }

      "a nested cons expression" in {
        val serializedExpression = "cons(1, cons(2, cons(3, empty)))"
        unsafeParseEvolution(serializedExpression) shouldBe
          cons(double(1), cons(double(2), cons(double(3), nil))).infer(Types.evolutionOfDoubles).toString
      }

      "a mapEmpty expression" in {
        val serializedExpression = """mapEmpty(cons(1, empty),cons(2, empty))"""
        unsafeParseEvolution(serializedExpression) shouldBe
          mapEmpty(cons(double(1), nil), cons(double(2), nil)).infer(Types.evolutionOfDoubles).toString
      }
    }
  }

  def expressions: Expressions[Types.F, ByVarParserK[Types.R, ?], noApi.Parser[String]] =
    EvolutionGrammar.parserGrammar(interpreter)

  def unsafeParseEvolution(expression: String): String =
    expressions.evolutionOfDoubles.parser(Nil).parse(expression).get.value.infer(Types.evolutionOfDoubles).toString
}
