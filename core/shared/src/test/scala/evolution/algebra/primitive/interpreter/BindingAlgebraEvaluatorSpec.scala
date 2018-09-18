package evolution.algebra.primitive.interpreter
import evolution.primitive.algebra.interpreter.{BindingAlgebraDebugEvaluator, BindingAlgebraEvaluator, Ctx}
import org.scalatest.{FreeSpec, Matchers}

class BindingAlgebraEvaluatorSpec extends FreeSpec with Matchers {
  val evaluator = BindingAlgebraEvaluator
  import evaluator._

  "a var0 expression" - {
    "evaluates to the first item in the stack" in {
      val expr = var0[Double]
      expr(List(() => 12)) shouldBe 12
    }
  }

  "a shift expression" - {
    "evaluates to the second item in the stack" in {
      val expr = shift(var0[Double])
      expr(List(() => 1, () => 2)) shouldBe 2
    }
  }

  "a lambda expression" - {
    "evaluates to a function" in {
      val expr = lambda[Int, Int]("x", var0[Int])
      expr(Nil)(13) shouldBe 13
    }
  }

  "an app expression" - {
    "evaluates a lambda" in {
      val expr = app(lambda[Int, Int]("x", var0), var0)
      expr(List(() => 1)) shouldBe 1
    }
  }

  "a let expression" - {
    "evaluates to the substitution of the evaluations" in {
      val expr = let[Int, Int]("x", value(1))(var0)
      expr(Nil) shouldBe 1
    }
  }

  "a fix expression" - {
    "evaluates recursively" in {
      pending
      def factorial(ctx: List[() => Any]): Int => Int =
        n => if (n <= 0) 1 else n * var0[Int => Int](ctx)(n - 1)

//      val expr = fix[Int => Int](factorial)
//      expr(Nil)(0) shouldBe 1
//      expr(Nil)(4) shouldBe 24
    }
  }

  def value[T](t: T): Ctx[T] = _ => t
}
