package evolution.language
import cats.Group
import evolution.data.ExpressionModule
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import evolution.typeclass.VectorSpace._

trait DesugarModule[F[_]] { self: ExpressionModule[F] =>
  import Expr._
  object Desugarer {

    def minus[T: Group](a: Expr[T], b: Expr[T]): Expr[T] =
      Add(a, Inverse(b))

    def inverseEvo[T: Group](t: Expr[F[T]]): Expr[F[T]] =
      Map(t, Lambda("t", Inverse(Var("t"))))

    def liftedPoint(x: Expr[F[Double]], y: Expr[F[Double]]): Expr[F[Point]] =
      ZipWith(x, y, lambda2[Double, Double, Point]("fx", "fy", Pnt(Var("fx"), Var("fy"))))

    def solve2[X: VectorSpace](eq: Expr[F[X => X => X]], x0: Expr[X], v0: Expr[X]): Expr[F[X]] =
      app3(solve2Lambda[X], eq, x0, v0)

    def mapWithDerivative[X: VectorSpace, Y]: Expr[(X => X => Y) => F[X] => F[Y]] =
      Lambda[X => X => Y, F[X] => F[Y]](
        "f",
        Fix[F[X] => F[Y]](
          lambda2(
            "self",
            "fx",
            MapCons[X, Y](
              Var("fx"),
              lambda2(
                "head1",
                "tail1",
                MapCons[X, Y](
                  Var("tail1"),
                  lambda2(
                    "head2",
                    "tail2",
                    Cons[Y](
                      app2[X, X, Y](Var("f"), Var("head1"), minus[X](Var("head2"), Var("head1"))),
                      App[F[X], F[Y]](Var("self"), Cons(Var("head2"), Var("tail2")))
                    )
                  )
                )
              )
            )
          )
        )
      )

    def derive[X: VectorSpace]: Expr[F[X] => F[X]] =
      App(mapWithDerivative[X, X], lambda2[X, X, X]("x", "v", Var("v")))

    def derive2[X: VectorSpace]: Expr[F[X] => F[X]] =
      Lambda("fx", App(derive[X], App(derive[X], Var[F[X]]("fx"))))

    def takeUntil[T](fa: Expr[F[T]], p: Expr[T => Boolean]): Expr[F[T]] = {
      val t = p.freshVarName("t")
      App(takeWhileLambda(Lambda(t, Not(App(p, Var[T](t))))), fa)
    }

    def norm(point: Expr[Point]): Expr[Double] =
      Exp(Add(Exp(X(point), Dbl(2)), Exp(Y(point), Dbl(2))), Dbl(0.5))

    def versor(point: Expr[Point]): Expr[Point] =
      Multiply(Div(Dbl(1), norm(point)), point)

    def withFirst[T1, T2](expr: Expr[F[T1]], f: Expr[T1 => F[T2]]): Expr[F[T2]] = {
      val (head, tail) = f.freshVarName2("head", "tail")
      MapCons(expr, lambda2[T1, F[T1], F[T2]](head, tail, App(f, Var[T1](head))))
    }

    def withFirst2[T1, T2](expr: Expr[F[T1]], f: Expr[T1 => T1 => F[T2]]): Expr[F[T2]] = {
      val (head1, tail1, head2, tail2) = f.freshVarName4("head1", "tail1", "head2", "tail2")
      MapCons(
        expr,
        lambda2[T1, F[T1], F[T2]](
          head1,
          tail1,
          MapCons(Var[F[T1]](tail1), lambda2[T1, F[T1], F[T2]](head2, tail2, app2(f, Var(head1), Var(head2))))
        )
      )
    }

    def withFirst3[T1, T2](expr: Expr[F[T1]], f: Expr[T1 => T1 => T1 => F[T2]]): Expr[F[T2]] = {
      val (head1, tail1, head2, tail2) = f.freshVarName4("head1", "tail1", "head2", "tail2")
      val (head3, tail3) = f.freshVarName2("head3", "tail3")
      MapCons(
        expr,
        lambda2[T1, F[T1], F[T2]](
          head1,
          tail1,
          MapCons(
            Var[F[T1]](tail1),
            lambda2[T1, F[T1], F[T2]](
              head2,
              tail2,
              MapCons(
                Var[F[T1]](tail2),
                lambda2[T1, F[T1], F[T2]](head3, tail3, app3(f, Var(head1), Var(head2), Var(head3)))
              )
            )
          )
        )
      )
    }

    private def solve2Lambda[X: VectorSpace]: Expr[F[X => X => X] => X => X => F[X]] =
      Fix[F[X => X => X] => X => X => F[X]](
        Lambda(
          "self",
          lambda3[F[X => X => X], X, X, F[X]](
            "a",
            "x0",
            "v0",
            Cons(
              Var("x0"),
              MapCons(
                Var[F[X => X => X]]("a"),
                lambda2[X => X => X, F[X => X => X], F[X]](
                  "aHead",
                  "aTail",
                  app3[F[X => X => X], X, X, F[X]](
                    Var("self"),
                    Var("aTail"),
                    Add(Var[X]("x0"), Var("v0")),
                    Add(
                      Var("v0"),
                      app2[X, X, X](Var("aHead"), Var("x0"), Var("v0"))
                    )
                  )
                )
              )
            )
          )
        )
      )

    private def takeWhileLambda[T](p: Expr[T => Boolean]): Expr[F[T] => F[T]] = {
      val (self, fa, head, tail) = p.freshVarName4("self", "fa", "head", "tail")
      Fix(
        lambda2(
          self,
          fa,
          MapCons(
            Var[F[T]](fa),
            lambda2[T, F[T], F[T]](
              head,
              tail,
              IfThen(
                App(p, Var[T](head)),
                Cons(Var(head), App(Var[F[T] => F[T]](self), Var[F[T]](tail))),
                Empty()
              )
            )
          )
        )
      )
    }

    def lambda2[A, B, C](var1: String, var2: String, expr: Expr[C]): Expr[A => B => C] =
      Lambda[A, B => C](var1, Lambda[B, C](var2, expr))

    def lambda3[A, B, C, D](var1: String, var2: String, var3: String, expr: Expr[D]): Expr[A => B => C => D] =
      Lambda[A, B => C => D](var1, lambda2[B, C, D](var2, var3, expr))

    private def app2[A, B, C](f: Expr[A => B => C], a: Expr[A], b: Expr[B]): Expr[C] =
      App(App(f, a), b)

    private def app3[A, B, C, D](f: Expr[A => B => C => D], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[D] =
      App(App(App(f, a), b), c)
  }
}
