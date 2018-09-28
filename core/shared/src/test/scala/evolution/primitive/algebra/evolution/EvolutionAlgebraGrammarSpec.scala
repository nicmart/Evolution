package evolution.primitive.algebra.evolution
import evolution.primitive.parser.CommonTestParsers
import org.scalatest.{FreeSpec, Matchers}

class EvolutionAlgebraGrammarSpec extends FreeSpec with Matchers with CommonTestParsers {
  "An Evolution Grammar" - {
    "should parse" - {
      "an empty evolution of Doubles" in {
        val serializedExpression = "empty"

      }

      "an empty evolution of Points" in {}

      "an evolution with a single Double" in {}

      "an evolution with a single point" in {}
    }
  }
}
