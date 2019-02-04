package evolution.primitive.algebra.binding.interpreter

import evolution.data.EvaluationContext._
import evolution.data.EvaluationModule._
import org.scalatest.{ FreeSpec, Matchers }

class BindingEvaluatorSpec extends FreeSpec with Matchers {
  import initial._
  "a var0 expression" - {
    "evaluates to the first item in the stack" in {
      val expr = Var0[Double]("x")
      materializeConstantWith(interpret(expr), ctxOf(12)) shouldBe 12
    }
  }

  "a shift expression" - {
    "evaluates to the second item in the stack" in {
      val expr = Shift(Var0[Double]("x"))
      materializeConstantWith(interpret(expr), ctxOf(1, 2)) shouldBe 2
    }
  }

  "a lambda expression" - {
    "evaluates to a function" in {
      val expr = Lambda[Int, Int]("x", Var0[Int]("x"))
      materializeConstant(interpret(expr))(13) shouldBe 13
    }
  }

  "an app expression" - {
    "evaluates a lambda" in {
      val expr = App(Lambda[Int, Int]("x", Var0("x")), Var0("y"))
      materializeConstantWith(interpret(expr), ctxOf(1)) shouldBe 1
    }
  }

  "a let expression" - {
    "evaluates to the substitution of the evaluations" in {
      val expr = Let[Double, Double]("x", Dbl(1), Var0("x"))
      materializeConstant(interpret(expr)) shouldBe 1
    }
  }
}
