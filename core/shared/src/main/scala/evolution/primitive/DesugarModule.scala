package evolution.primitive
import cats.Semigroup
import cats.Group
import evolution.data.WithInitial
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import evolution.typeclass.VectorSpace._
import cats.implicits._

trait DesugarModule[F[_]] { self: WithInitial[F] =>
  import initial._

  object Desugarer {
    def constant[A](a: R[A]): R[F[A]] =
      Fix[F[A]](Lambda("self", Cons(Shift(a), varN("self", 0))))

    def zipWith[A, B, C](a: R[F[A]], b: R[F[B]], f: R[A => B => C]): R[F[C]] =
      app2(zipWithLambda(f), a, b)

    def addEvo[T: Semigroup](a: R[F[T]], b: R[F[T]]): R[F[T]] =
      zipWith(a, b, lambda2[T, T, T]("a", "b", Add[T](varN("a", 1), varN("b", 0))))

    def multEvo[T: VectorSpace](k: R[F[Double]], t: R[F[T]]): R[F[T]] =
      zipWith(k, t, lambda2[Double, T, T]("k", "t", Multiply[T](varN("k", 1), varN("t", 0))))

    def inverseEvo[T: Group](t: R[F[T]]): R[F[T]] =
      map(t, Lambda("t", Inverse(varN("t", 0))))

    def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]] =
      app2(zipWithLambda(lambda2[Double, Double, Point]("fx", "fy", Pnt(varN("fx", 1), varN("fy", 0)))), x, y)

    def polar(radius: R[F[Double]], angle: R[F[Double]]): R[F[Point]] =
      app2(
        zipWithLambda(
          lambda2[Double, Double, Point](
            "radius",
            "angle",
            Multiply(varN("radius", 1), Pnt(Cos(varN("angle", 0)), Sin(varN("angle", 0))))
          )
        ),
        radius,
        angle
      )

    def concat[A](fa1: R[F[A]], fa2: R[F[A]]): R[F[A]] =
      app2(concatLambda, fa1, fa2)

    def integrate[A: VectorSpace](start: R[A], speed: R[F[A]]): R[F[A]] =
      app2(integrateLambda[A], start, speed)

    def solve1[X: VectorSpace](eq: R[F[X => X]], x0: R[X]): R[F[X]] =
      app2(solve1Lambda[X], eq, x0)

    def solve2[X: VectorSpace](eq: R[F[X => X => X]], x0: R[X], v0: R[X]): R[F[X]] =
      app3(solve2Lambda[X], eq, x0, v0)

    def map[A, B](fa: R[F[A]], f: R[A => B]): R[F[B]] =
      App(mapLambda(f), fa)

    def flatMap[A, B](fa: R[F[A]], f: R[A => F[B]]): R[F[B]] =
      App(flatMapLambda(f), fa)

    // TODO f as parameter of lambda, so we can remove shiftN
    private def mapLambda[A, B](f: R[A => B]): R[F[A] => F[B]] =
      Fix[F[A] => F[B]](
        Lambda(
          "self",
          Lambda[F[A], F[B]](
            "fa",
            MapCons[A, B](
              varN[F[A]]("fa", 0),
              lambda2(
                "head",
                "tail",
                Cons(App(shiftN(f, 4), varN[A]("head", 1)), App(varN[F[A] => F[B]]("self", 3), varN[F[A]]("tail", 0)))
              )
            )
          )
        )
      )

    // TODO f as parameter of lambda, so we can remove shiftN
    private def flatMapLambda[A, B](f: R[A => F[B]]): R[F[A] => F[B]] =
      Fix[F[A] => F[B]](
        Lambda(
          "self",
          Lambda(
            "fa",
            MapCons[A, B](
              varN[F[A]]("fa", 0),
              lambda2[A, F[A], F[B]](
                "head",
                "tail",
                concat(
                  App(shiftN(f, 4), varN[A]("head", 1)),
                  App(varN[F[A] => F[B]]("self", 3), varN[F[A]]("tail", 0))
                )
              )
            )
          )
        )
      )

    private def concatLambda[A]: R[F[A] => F[A] => F[A]] =
      Fix[F[A] => F[A] => F[A]](
        Lambda(
          "self",
          lambda2[F[A], F[A], F[A]](
            "fa1",
            "fa2",
            MapEmpty[A](
              MapCons[A, A](
                varN[F[A]]("fa1", 1),
                lambda2(
                  "head1",
                  "tail1",
                  Cons(
                    varN[A]("head1", 1),
                    app2(
                      varN[F[A] => F[A] => F[A]]("self", 4),
                      varN[F[A]]("tail1", 0),
                      varN[F[A]]("fa2", 2)
                    )
                  )
                )
              ),
              varN[F[A]]("fa2", 0)
            )
          )
        )
      )

    private def integrateLambda[T: VectorSpace]: R[T => F[T] => F[T]] =
      Fix[T => F[T] => F[T]](
        Lambda(
          "self",
          lambda2[T, F[T], F[T]](
            "start",
            "speed",
            MapCons(
              varN[F[T]]("speed", 0),
              lambda2[T, F[T], F[T]](
                "speedHead",
                "speedTail",
                Cons(
                  varN("start", 3),
                  app2[T, F[T], F[T]](
                    varN("self", 4),
                    Add[T](varN[T]("start", 3), varN[T]("speedHead", 1)),
                    varN[F[T]]("speedTail", 0)
                  )
                )
              )
            )
          )
        )
      )

    private def solve1Lambda[X: VectorSpace]: R[F[X => X] => X => F[X]] =
      Fix[F[X => X] => X => F[X]](
        Lambda(
          "self",
          lambda2[F[X => X], X, F[X]](
            "v",
            "x0",
            Cons(
              varN("x0", 0),
              MapCons(
                varN[F[X => X]]("v", 1),
                lambda2[X => X, F[X => X], F[X]](
                  "vHead",
                  "vTail",
                  app2[F[X => X], X, F[X]](
                    varN("self", 4),
                    varN("vTail", 0),
                    Add(varN("x0", 2), App[X, X](varN("vHead", 1), varN("x0", 2)))
                  )
                )
              )
            )
          )
        )
      )

    private def solve2Lambda[X: VectorSpace]: R[F[X => X => X] => X => X => F[X]] =
      Fix[F[X => X => X] => X => X => F[X]](
        Lambda(
          "self",
          lambda3[F[X => X => X], X, X, F[X]](
            "a",
            "x0",
            "v0",
            Cons(
              varN("x0", 1),
              MapCons(
                varN[F[X => X => X]]("a", 2),
                lambda2[X => X => X, F[X => X => X], F[X]](
                  "aHead",
                  "aTail",
                  app3[F[X => X => X], X, X, F[X]](
                    varN("self", 5),
                    varN("aTail", 0),
                    Add(varN("x0", 3), varN("v0", 2)),
                    Add(
                      varN("v0", 2),
                      app2[X, X, X](varN("aHead", 1), varN("x0", 3), varN("v0", 2))
                    )
                  )
                )
              )
            )
          )
        )
      )

    def take[T](n: R[Int], ft: R[F[T]]): R[F[T]] =
      app2(takeLambda, n, ft)

    private def takeLambda[T]: R[Int => F[T] => F[T]] =
      Fix[Int => F[T] => F[T]](
        Lambda(
          "self",
          Lambda(
            "n",
            Lambda(
              "ft",
              IfThen(
                Equals[Int](Integer(0), varN[Int]("n", 1)),
                Empty[T](),
                MapCons[T, T](
                  varN[F[T]]("ft", 0),
                  Lambda(
                    "head",
                    Lambda(
                      "tail",
                      Cons(
                        varN[T]("head", 1),
                        app2(
                          varN[Int => F[T] => F[T]]("self", 4),
                          Add(varN[Int]("n", 3), Inverse(Integer(1))),
                          varN[F[T]]("ft", 0)
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

    private def lambda2[A, B, C](var1: String, var2: String, expr: R[C]): R[A => B => C] =
      Lambda[A, B => C](var1, Lambda[B, C](var2, expr))

    private def lambda3[A, B, C, D](var1: String, var2: String, var3: String, expr: R[D]): R[A => B => C => D] =
      Lambda[A, B => C => D](var1, lambda2[B, C, D](var2, var3, expr))

    private def app2[A, B, C](f: R[A => B => C], a: R[A], b: R[B]): R[C] =
      App(App(f, a), b)

    private def app3[A, B, C, D](f: R[A => B => C => D], a: R[A], b: R[B], c: R[C]): R[D] =
      App(App(App(f, a), b), c)

    private def varN[A](name: String, n: Int): R[A] = shiftN(Var0(name), n)
    private def shiftN[A](expr: R[A], n: Int): R[A] = if (n <= 0) expr else Shift(shiftN(expr, n - 1))

    private def zipWithLambda[A, B, C](f: R[A => B => C]): R[F[A] => F[B] => F[C]] =
      Fix[F[A] => F[B] => F[C]](
        Lambda[F[A] => F[B] => F[C], F[A] => F[B] => F[C]](
          "self",
          lambda2[F[A], F[B], F[C]](
            "fa",
            "fb",
            MapCons[A, C](
              varN[F[A]]("fa", 1),
              lambda2(
                "aHead",
                "aTail",
                MapCons[B, C](
                  varN[F[B]]("fb", 2),
                  lambda2(
                    "bHead",
                    "bTail",
                    Cons(
                      app2(f, varN[A]("aHead", 3), varN[B]("bHead", 1)),
                      app2(varN[F[A] => F[B] => F[C]]("self", 6), varN[F[A]]("aTail", 2), varN[F[B]]("bTail", 0))
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
