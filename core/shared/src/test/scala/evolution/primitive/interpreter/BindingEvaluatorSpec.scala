package evolution.primitive.interpreter
import evolution.primitive.algebra.binding.interpreter.{BindingEvaluator, EvaluationResult, Value}
import org.scalatest.{FreeSpec, Matchers}

class BindingEvaluatorSpec extends FreeSpec with Matchers {
  val evaluator = BindingEvaluator
  import evaluator._

  "a var0 expression" - {
    "evaluates to the first item in the stack" in {
      val expr = var0[Double]
      expr.get(List(() => 12)) shouldBe 12
    }
  }

  "a shift expression" - {
    "evaluates to the second item in the stack" in {
      val expr = shift(var0[Double])
      expr.get(List(() => 1, () => 2)) shouldBe 2
    }
  }

  "a lambda expression" - {
    "evaluates to a function" in {
      val expr = lambda[Int, Int]("x", var0[Int])
      expr.get(Nil)(13) shouldBe 13
    }
  }

  "an app expression" - {
    "evaluates a lambda" in {
      val expr = app(lambda[Int, Int]("x", var0), var0)
      expr.get(List(() => 1)) shouldBe 1
    }
  }

  "a let expression" - {
    "evaluates to the substitution of the evaluations" in {
      val expr = let[Int, Int]("x", value(1), var0)
      expr.get(Nil) shouldBe 1
    }
  }

  def value[T](t: T): EvaluationResult[T] = Value(_ => t)
}
