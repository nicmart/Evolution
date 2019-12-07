package evolution.compiler.term

import evolution.compiler.LanguageSpec
import org.scalatest.FreeSpec
import Term._
import Term.Literal._

import scala.util.Try

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

  "identifiers" - {
    "in the register" in {
      val term = Id("x")
      val interpreter = RegisterBasedInterpreter.fresh
      interpreter.bind("x", 12345)

      interpreter.interpret(term) shouldBe 12345
    }

    "not in the register" in {
      val term = Id("x")
      val interpreter = RegisterBasedInterpreter.fresh

      Try(interpreter.interpret(term)).isFailure shouldBe true
    }
  }

  lazy val interpreter = new TermInterpreter
}
