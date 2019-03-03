package evolution.data

import evolution.data.EvaluationContext._
import evolution.data.EvaluationModule._
import cats.implicits._
import Expr._
import org.scalatest.{ FreeSpec, Matchers }

class EvaluationSpec extends FreeSpec with Matchers {
  type F[T] = EvoRepr[T]
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

  "should correctly create recursive evolutions" in {
    val expr: Expr[F[Double]] = Fix(Lambda[F[Double], F[Double]]("x", Cons(Dbl(1), Var0[F[Double]]("x"))))
    val stream = materializeExpr(0, expr)
    stream.take(10).toList shouldBe List.fill(10)(1.0)
  }

  "should create an evolution of a sequence of integers" in {
    val expr: Expr[F[Double]] =
      App[Double, F[Double]](
        Fix(
          Lambda(
            "f",
            Lambda("s", Cons(Var0("s"), App(Shift(Var0[Double => F[Double]]("f")), Add(Var0[Double]("s"), Dbl(1)))))
          )
        ),
        Dbl(0)
      )

    val stream = materializeExpr(0, expr)
    stream.take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
}
