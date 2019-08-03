package evolution.data

import evolution.data.EvaluationContext._
import evolution.data.EvaluationModule._
import cats.implicits._
import Expr._
import cats.kernel.Semigroup
import org.scalatest.{ FreeSpec, Matchers }
import evolution.typeclass.Semigroupoid

class EvaluationSpec extends FreeSpec with Matchers {
  type F[T] = EvoRepr[T]
  "a var0 expression" - {
    "evaluates to the first item in the stack" in {
      val expr = Var[Double]("x")
      materializeConstantWith(interpret(expr), ctxOf("x" -> 12)) shouldBe 12
    }
  }

  "a lambda expression" - {
    "evaluates to a function" in {
      val expr = Lambda[Int, Int]("x", Var[Int]("x"))
      materializeConstant(interpret(expr))(13) shouldBe 13
    }
  }

  "an app expression" - {
    "evaluates a lambda" in {
      val expr = App(Lambda[Int, Int]("x", Var[Int]("x")), Var[Int]("y"))
      materializeConstantWith(interpret(expr), ctxOf("y" -> 1)) shouldBe 1
    }
  }

  "a let expression" - {
    "evaluates to the substitution of the evaluations" in {
      val expr = Let[Double, Double]("x", Dbl(1), Var("x"))
      materializeConstant(interpret(expr)) shouldBe 1
    }
  }

  "should correctly create recursive evolutions" in {
    val expr: Expr[F[Double]] = Fix(Lambda[F[Double], F[Double]]("x", Cons(Dbl(1), Var[F[Double]]("x"))))
    val stream = materializeExpr(0, expr)
    stream.take(10).toList shouldBe List.fill(10)(1.0)
  }

  "should create an evolution of a sequence of integers" in {
    val expr: Expr[F[Double]] =
      App[Double, F[Double]](
        Fix(
          Lambda(
            "f",
            Lambda(
              "s",
              Cons(Var("s"), App(Var[Double => F[Double]]("f"), Add(Var[Double]("s"), Dbl(1), Semigroupoid.Additive.dblDblDbl)))
            )
          )
        ),
        Dbl(0)
      )

    val stream = materializeExpr(0, expr)
    stream.take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }
}
