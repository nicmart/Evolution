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

  "lambdas" - {
    "identity" in {
      val term = Lambda("x", Id("x"))

      val interpreted = interpreter.interpret(term).asInstanceOf[Any => Any]

      interpreted("anything") shouldBe "anything"
      interpreted(12345) shouldBe 12345
    }

    "with multiple vars" in {
      val term = Lambda("x", Lambda("y", Id("x")))

      val interpreted = interpreter.interpret(term).asInstanceOf[Any => Any => Any]

      interpreted("first")("second") shouldBe "first"
      interpreted(1)(2) shouldBe 1
    }
  }

  "apps" - {
    "of single argument" in {
      val term = App(Id("f"), Lit(LitDouble(1)))
      val interpreter = RegisterBasedInterpreter.fresh
      interpreter.bind("f", (x: Double) => x + 1)

      interpreter.interpret(term) shouldBe 2
    }

    "of multiple arguments" in {
      val term = App(App(Id("f"), Lit(LitDouble(1))), Lit(LitDouble(2)))
      val interpreter = RegisterBasedInterpreter.fresh
      interpreter.bind("f", (x: Double) => (y: Double) => x + y)

      interpreter.interpret(term) shouldBe 3
    }
  }

  lazy val interpreter = new TermInterpreter
}
