package evolution.compiler.impl.evaluation

import evolution.data.EvaluationContext._
import evolution.compiler.expression.Expr._
import evolution.compiler.expression.Expr
import org.scalatest.{ FreeSpec, Matchers }
import evolution.materialization.Evolution
import evolution.compiler.impl.evaluation.EvalMaterializer.materializeExpr
import evolution.compiler.impl.evaluation.model.Contextual
import evolution.compiler.expression.typeclass.Additive

final class EvaluationSpec extends FreeSpec with Matchers {
  type F[T] = Evolution[T]
  "a var0 expression" - {
    "evaluates to the first item in the stack" in {
      val expr = Var[Double]("x")
      materializeConstantWith(materializeExpr(expr), ctxOf("x" -> 12)) shouldBe 12
    }
  }

  "a lambda expression" - {
    "evaluates to a function" in {
      val expr = Lambda[Int, Int]("x", Var[Int]("x"))
      materializeConstant(materializeExpr(expr))(13) shouldBe 13
    }
  }

  "an app expression" - {
    "evaluates a lambda" in {
      val expr = App(Lambda[Int, Int]("x", Var[Int]("x")), Var[Int]("y"))
      materializeConstantWith(materializeExpr(expr), ctxOf("y" -> 1)) shouldBe 1
    }
  }

  "a let expression" - {
    "evaluates to the substitution of the evaluations" in {
      val expr = Let[Double, Double]("x", Dbl(1), Var("x"))
      materializeConstant(materializeExpr(expr)) shouldBe 1
    }
  }

  "should correctly create recursive evolutions" in {
    val expr: Expr[F[Double]] = Fix(Lambda[F[Double], F[Double]]("x", Cons(Dbl(1), Var[F[Double]]("x"))))
    val stream = materializeIterator(0, expr)
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
              Cons(
                Var("s"),
                App[Double, F[Double]](
                  Var[Double => F[Double]]("f"),
                  Add(Var[Double]("s"), Dbl(1), Additive.DoubleDoubleDouble)
                )
              )
            )
          )
        ),
        Dbl(0)
      )

    val stream = materializeIterator(0, expr)
    stream.take(10).toList shouldBe List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  def materializeConstant[T](t: Contextual[T]): T = materializeConstantWith(t, emptyCtx)
  def materializeConstantWith[T](t: Contextual[T], ctx: Ctx): T = t(ctx)
  def materializeIterator[T](seed: Long, expr: Expr[Evolution[T]]): Iterator[T] = {
    Evolution.setSeed(seed)
    materializeExpr(expr).apply(emptyCtx).run
  }
}
