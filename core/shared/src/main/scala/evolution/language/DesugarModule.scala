package evolution.language
import cats.{ Group, Semigroup }
import cats.implicits._
import evolution.data.ExpressionModule
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import evolution.typeclass.VectorSpace._

trait DesugarModule[F[_]] { self: ExpressionModule[F] =>
  import Expr._
  object Desugarer {
    def constant[A](a: Expr[A]): Expr[F[A]] = {
      val self = a.freshVarName("self")
      Fix[F[A]](Lambda(self, Cons(a, Var(self))))
    }

    def zipWith[A, B, C](a: Expr[F[A]], b: Expr[F[B]], f: Expr[A => B => C]): Expr[F[C]] =
      app2(zipWithLambda(f), a, b)

    def liftedAdd[T: Semigroup](a: Expr[F[T]], b: Expr[F[T]]): Expr[F[T]] =
      zipWith(a, b, lambda2[T, T, T]("a", "b", Add[T](Var("a"), Var("b"))))

    def liftedMult[T: VectorSpace](k: Expr[F[Double]], t: Expr[F[T]]): Expr[F[T]] =
      zipWith(k, t, lambda2[Double, T, T]("k", "t", Multiply[T](Var("k"), Var("t"))))

    def inverseEvo[T: Group](t: Expr[F[T]]): Expr[F[T]] =
      map(t, Lambda("t", Inverse(Var("t"))))

    def liftedPoint(x: Expr[F[Double]], y: Expr[F[Double]]): Expr[F[Point]] =
      app2(zipWithLambda(lambda2[Double, Double, Point]("fx", "fy", Pnt(Var("fx"), Var("fy")))), x, y)

    def polar(radius: Expr[Double], angle: Expr[Double]): Expr[Point] =
      Multiply(radius, Pnt(Cos(angle), Sin(angle)))

    def liftedPolar(radius: Expr[F[Double]], angle: Expr[F[Double]]): Expr[F[Point]] =
      app2(
        zipWithLambda(
          lambda2[Double, Double, Point](
            "radius",
            "angle",
            polar(Var("radius"), Var("angle"))
          )
        ),
        radius,
        angle
      )

    def concat[A](fa1: Expr[F[A]], fa2: Expr[F[A]]): Expr[F[A]] =
      app2(concatLambda, fa1, fa2)

    def integrate[A: VectorSpace](start: Expr[A], speed: Expr[F[A]]): Expr[F[A]] =
      app2(integrateLambda[A], start, speed)

    def solve1[X: VectorSpace](eq: Expr[F[X => X]], x0: Expr[X]): Expr[F[X]] =
      app2(solve1Lambda[X], eq, x0)

    def solve2[X: VectorSpace](eq: Expr[F[X => X => X]], x0: Expr[X], v0: Expr[X]): Expr[F[X]] =
      app3(solve2Lambda[X], eq, x0, v0)

    def map[A, B](fa: Expr[F[A]], f: Expr[A => B]): Expr[F[B]] =
      App(mapLambda(f), fa)

    def flatMap[A, B](fa: Expr[F[A]], f: Expr[A => F[B]]): Expr[F[B]] =
      App(flatMapLambda(f), fa)

    // TODO f as parameter of lambda, so we can remove shiftN
    private def mapLambda[A, B](f: Expr[A => B]): Expr[F[A] => F[B]] = {
      val (self, fa, head, tail) = f.freshVarName4("self", "fa", "head", "tail")
      Fix[F[A] => F[B]](
        Lambda(
          self,
          Lambda[F[A], F[B]](
            fa,
            MapCons[A, B](
              Var[F[A]](fa),
              lambda2(
                head,
                tail,
                Cons(App(f, Var[A](head)), App(Var[F[A] => F[B]](self), Var[F[A]](tail)))
              )
            )
          )
        )
      )
    }

    // TODO f as parameter of lambda, so we can remove shiftN
    private def flatMapLambda[A, B](f: Expr[A => F[B]]): Expr[F[A] => F[B]] = {
      val (self, fa, head, tail) = f.freshVarName4("self", "fa", "head", "tail")
      Fix[F[A] => F[B]](
        Lambda(
          self,
          Lambda(
            fa,
            MapCons[A, B](
              Var[F[A]](fa),
              lambda2[A, F[A], F[B]](
                head,
                tail,
                concat(
                  App(f, Var[A](head)),
                  App(Var[F[A] => F[B]](self), Var[F[A]](tail))
                )
              )
            )
          )
        )
      )
    }

    private def concatLambda[A]: Expr[F[A] => F[A] => F[A]] =
      Fix[F[A] => F[A] => F[A]](
        Lambda(
          "self",
          lambda2[F[A], F[A], F[A]](
            "fa1",
            "fa2",
            MapEmpty[A](
              MapCons[A, A](
                Var[F[A]]("fa1"),
                lambda2(
                  "head1",
                  "tail1",
                  Cons(
                    Var[A]("head1"),
                    app2(
                      Var[F[A] => F[A] => F[A]]("self"),
                      Var[F[A]]("tail1"),
                      Var[F[A]]("fa2")
                    )
                  )
                )
              ),
              Var[F[A]]("fa2")
            )
          )
        )
      )

    private def integrateLambda[T: VectorSpace]: Expr[T => F[T] => F[T]] =
      Fix[T => F[T] => F[T]](
        Lambda(
          "self",
          lambda2[T, F[T], F[T]](
            "start",
            "speed",
            MapCons(
              Var[F[T]]("speed"),
              lambda2[T, F[T], F[T]](
                "speedHead",
                "speedTail",
                Cons(
                  Var("start"),
                  app2[T, F[T], F[T]](
                    Var("self"),
                    Add[T](Var[T]("start"), Var[T]("speedHead")),
                    Var[F[T]]("speedTail")
                  )
                )
              )
            )
          )
        )
      )

    private def solve1Lambda[X: VectorSpace]: Expr[F[X => X] => X => F[X]] =
      Fix[F[X => X] => X => F[X]](
        Lambda(
          "self",
          lambda2[F[X => X], X, F[X]](
            "v",
            "x0",
            Cons(
              Var("x0"),
              MapCons(
                Var[F[X => X]]("v"),
                lambda2[X => X, F[X => X], F[X]](
                  "vHead",
                  "vTail",
                  app2[F[X => X], X, F[X]](
                    Var("self"),
                    Var("vTail"),
                    Add(Var("x0"), App[X, X](Var("vHead"), Var("x0")))
                  )
                )
              )
            )
          )
        )
      )

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
                    Add(Var("x0"), Var("v0")),
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

    def take[T](n: Expr[Int], ft: Expr[F[T]]): Expr[F[T]] =
      app2(takeLambda, n, ft)

    private def takeLambda[T]: Expr[Int => F[T] => F[T]] =
      Fix[Int => F[T] => F[T]](
        Lambda(
          "self",
          Lambda(
            "n",
            Lambda(
              "ft",
              IfThen(
                Equals[Int](Integer(0), Var[Int]("n")),
                Empty[T](),
                MapCons[T, T](
                  Var[F[T]]("ft"),
                  Lambda(
                    "head",
                    Lambda(
                      "tail",
                      Cons(
                        Var[T]("head"),
                        app2(
                          Var[Int => F[T] => F[T]]("self"),
                          Add(Var[Int]("n"), Inverse(Integer(1))),
                          Var[F[T]]("tail")
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

    private def lambda2[A, B, C](var1: String, var2: String, expr: Expr[C]): Expr[A => B => C] =
      Lambda[A, B => C](var1, Lambda[B, C](var2, expr))

    private def lambda3[A, B, C, D](var1: String, var2: String, var3: String, expr: Expr[D]): Expr[A => B => C => D] =
      Lambda[A, B => C => D](var1, lambda2[B, C, D](var2, var3, expr))

    private def app2[A, B, C](f: Expr[A => B => C], a: Expr[A], b: Expr[B]): Expr[C] =
      App(App(f, a), b)

    private def app3[A, B, C, D](f: Expr[A => B => C => D], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[D] =
      App(App(App(f, a), b), c)

    private def zipWithLambda[A, B, C](f: Expr[A => B => C]): Expr[F[A] => F[B] => F[C]] = {
      val (self, fa, fb) = f.freshVarName3("self", "fa", "fb")
      val (aHead, bHead, aTail, bTail) = f.freshVarName4("aHead", "bHead", "aTail", "bTail")
      Fix[F[A] => F[B] => F[C]](
        Lambda[F[A] => F[B] => F[C], F[A] => F[B] => F[C]](
          self,
          lambda2[F[A], F[B], F[C]](
            fa,
            fb,
            MapCons[A, C](
              Var[F[A]](fa),
              lambda2(
                aHead,
                aTail,
                MapCons[B, C](
                  Var[F[B]](fb),
                  lambda2(
                    bHead,
                    bTail,
                    Cons(
                      app2(f, Var[A](aHead), Var[B](bHead)),
                      app2(Var[F[A] => F[B] => F[C]](self), Var[F[A]](aTail), Var[F[B]](bTail))
                    )
                  )
                )
              )
            )
          )
        )
      )
    }
  }
}
