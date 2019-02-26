package evolution.primitive.algebra.evolution.interpreter

import cats.instances.double._
import evolution.data.EvaluationModule._
import org.scalatest.{ FreeSpec, Matchers }

class EvolutionEvaluatorSpec extends FreeSpec with Matchers {
  import expressionModule._
  type F[T] = EvoRepr[T]
  "The ToEvolution interpreter" - {
    "should correctly create recursive evolutions" in {
      val expr: Expr[F[Double]] = Fix(Lambda[F[Double], F[Double]]("x", Cons(Dbl(1), Var0[F[Double]]("x"))))
      val stream = materializeExpr(0, expr)
      stream.take(10).toList shouldBe List.fill(10)(1.0)
    }

    "should create an evolution of the sequence of integers" in {
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

    // TODO We need to take a decision on derived expressions!
    "should be able to map evolutions" in {
//      val expr: Expr[F[Double]] = initiMap(Uniform(Dbl(0), Dbl(1)), Lambda[Double, Double]("x", Dbl(0)))
//
//      val stream = materializeExpr(0L, expr)
//
//      stream.take(4).toList shouldBe List(0, 0, 0, 0)
    }

    "should be able to express integrations" in {
      pending
    }

    "should be to define constants" in {
      pending
//      val expr: Result[F[Double]] = constant(double(1))
//      val stream = materialize(0, expr)
//      stream.take(2).toList shouldBe List(1, 1)
    }

    "should be able combine two evolutions of doubles into one evolution of points" in {
      pending
//
//      val expr: Result[F[Point]] = cartesian(constant(double(1)), constant(double(2)))
//      val stream = materialize(0, expr)
//      stream.take(2).toList shouldBe List(Point(1, 2), Point(1, 2))
    }
  }
}
