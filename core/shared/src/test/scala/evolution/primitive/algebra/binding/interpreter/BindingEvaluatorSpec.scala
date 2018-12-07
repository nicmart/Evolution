package evolution.primitive.algebra.binding.interpreter

import evolution.data.Evaluation
import evolution.data.Evaluation._
import evolution.data.EvaluationContext._
import evolution.data.EvaluationModule._
import org.scalatest.{ FreeSpec, Matchers }

class BindingEvaluatorSpec extends FreeSpec with Matchers {
  val evaluator = interpreter.bind
  import evaluator._

  "a var0 expression" - {
    "evaluates to the first item in the stack" in {
      val expr = var0[Double]("x")
      materializeConstantWith(expr, ctxOf(12)) shouldBe 12
    }
  }

  "a shift expression" - {
    "evaluates to the second item in the stack" in {
      val expr = shift(var0[Double]("x"))
      materializeConstantWith(expr, ctxOf(1, 2)) shouldBe 2
    }
  }

  "a lambda expression" - {
    "evaluates to a function" in {
      val expr = lambda[Int, Int]("x", var0[Int]("x"))
      materializeConstant(expr)(13) shouldBe 13
    }
  }

  "an app expression" - {
    "evaluates a lambda" in {
      val expr = app(lambda[Int, Int]("x", var0("x")), var0("y"))
      materializeConstantWith(expr, ctxOf(1)) shouldBe 1
    }
  }

  "a let expression" - {
    "evaluates to the substitution of the evaluations" in {
      val expr = let[Double, Double]("x", interpreter.constants.double(1), var0("x"))
      materializeConstant(expr) shouldBe 1
    }
  }
}
