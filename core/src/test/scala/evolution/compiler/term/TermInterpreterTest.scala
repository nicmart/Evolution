package evolution.compiler.term

import evolution.compiler.LanguageSpec
import org.scalatest.FreeSpec
import Term._
import Term.Literal._

class TermInterpreterTest extends LanguageSpec {
  "literals" - {
    "integers" in {
      val term = Lit(LitInt(123))
      val result = interpreter.interpret(term)
      result shouldBe 123
    }

    "booleans" in {
      val term = Lit(LitBool(true))
      val result = interpreter.interpret(term)
      result shouldBe true
    }

    "doubles" in {
      val term = Lit(LitDouble(1.1))
      val result = interpreter.interpret(term)
      result shouldBe 1.1
    }
  }

  lazy val interpreter = new TermInterpreter
}
