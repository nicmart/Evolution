package evolution.primitive
import cats.Semigroup
import cats.Group
import evolution.data.WithExpression
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import evolution.typeclass.VectorSpace._
import cats.implicits._

trait DesugarModule[F[_]] { self: WithExpression[F] =>
  import expressionModule._, expressionModule.Constructors._

  object Desugarer {
    def constant[A]: Expr[A => F[A]] =
      Lambda("a", Fix[F[A]](Lambda[F[A], F[A]]("self", Cons[A](varN[A]("a", 1), varN[F[A]]("self", 0)))))

    def zipWith[A, B, C]: Expr[F[A] => F[B] => (A => B => C) => F[C]] =
      zipLambda[A, B, C]

    def addEvo[T: Semigroup]: Expr[F[T] => F[T] => F[T]] =
      zipWithLambda(lambda2[T, T, T]("a", "b", add[T](varN[T]("a", 1), varN[T]("b", 0))))

    def inverseEvo[T: Group]: Expr[F[T] => F[T]] =
      Lambda("ft", App2[F[T], T => T, F[T]](map, varN[F[T]]("ft", 0), Inverse[T]()))

    def cartesian: Expr[F[Double] => F[Double] => F[Point]] =
      zipWithLambda(lambda2[Double, Double, Point]("fx", "fy", pnt(varN("fx", 1), varN("fy", 0))))

    def polar: Expr[F[Double] => F[Double] => F[Point]] =
      zipWithLambda(
        lambda2[Double, Double, Point](
          "radius",
          "angle",
          multiply(varN("radius", 1), pnt(cos(varN("angle", 0)), sin(varN("angle", 0))))
        )
      )

    def concat[A]: Expr[F[A] => F[A] => F[A]] =
      concatLambda

    def integrate[A: VectorSpace]: Expr[A => F[A] => F[A]] =
      integrateLambda[A]

    def solve1[X: VectorSpace]: Expr[F[X => X] => X => F[X]] =
      solve1Lambda[X]

    def solve2[X: VectorSpace]: Expr[F[X => X => X] => X => X => F[X]] =
      solve2Lambda[X]

    def map[A, B]: Expr[F[A] => (A => B) => F[B]] =
      lambda2("fa", "f", App(mapLambda[A, B](varN("f", 0)), varN("fa", 1)))

    def flatMap[A, B]: Expr[F[A] => (A => F[B]) => F[B]] =
      lambda2("fa", "f", App(flatMapLambda[A, B](varN("f", 0)), varN("fa", 1)))

    def take[T]: Expr[Int => F[T] => F[T]] =
      takeLambda

    // TODO f as parameter of lambda, so we can remove shiftN
    private def mapLambda[A, B](f: Expr[A => B]): Expr[F[A] => F[B]] =
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
    private def flatMapLambda[A, B](f: Expr[A => F[B]]): Expr[F[A] => F[B]] =
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
                app2(
                  concat,
                  App(shiftN(f, 4), varN[A]("head", 1)),
                  App(varN[F[A] => F[B]]("self", 3), varN[F[A]]("tail", 0))
                )
              )
            )
          )
        )
      )

    private def concatLambda[A]: Expr[F[A] => F[A] => F[A]] =
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

    private def integrateLambda[T: VectorSpace]: Expr[T => F[T] => F[T]] =
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
                    add[T](varN[T]("start", 3), varN[T]("speedHead", 1)),
                    varN[F[T]]("speedTail", 0)
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
              varN("x0", 0),
              MapCons(
                varN[F[X => X]]("v", 1),
                lambda2[X => X, F[X => X], F[X]](
                  "vHead",
                  "vTail",
                  app2[F[X => X], X, F[X]](
                    varN("self", 4),
                    varN("vTail", 0),
                    add(varN("x0", 2), App[X, X](varN("vHead", 1), varN("x0", 2)))
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
              varN("x0", 1),
              MapCons(
                varN[F[X => X => X]]("a", 2),
                lambda2[X => X => X, F[X => X => X], F[X]](
                  "aHead",
                  "aTail",
                  app3[F[X => X => X], X, X, F[X]](
                    varN("self", 5),
                    varN("aTail", 0),
                    add[X](varN("x0", 3), varN("v0", 2)),
                    add(
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

    private def takeLambda[T]: Expr[Int => F[T] => F[T]] =
      Fix[Int => F[T] => F[T]](
        Lambda(
          "self",
          Lambda(
            "n",
            Lambda(
              "ft",
              ifThen(
                Constructors.equals[Int](Integer(0), varN[Int]("n", 1)),
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
                          add(varN[Int]("n", 3), inverse(Integer(1))),
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

    private def lambda2[A, B, C](var1: String, var2: String, expr: Expr[C]): Expr[A => B => C] =
      Lambda[A, B => C](var1, Lambda[B, C](var2, expr))

    private def lambda3[A, B, C, D](var1: String, var2: String, var3: String, expr: Expr[D]): Expr[A => B => C => D] =
      Lambda[A, B => C => D](var1, lambda2[B, C, D](var2, var3, expr))

    private def app2[A, B, C](f: Expr[A => B => C], a: Expr[A], b: Expr[B]): Expr[C] =
      App(App(f, a), b)

    private def app3[A, B, C, D](f: Expr[A => B => C => D], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[D] =
      App(App(App(f, a), b), c)

    private def varN[A](name: String, n: Int): Expr[A] = shiftN(Var0(name), n)
    private def shiftN[A](expr: Expr[A], n: Int): Expr[A] = if (n <= 0) expr else Shift(shiftN(expr, n - 1))

    private def zipLambda[A, B, C]: Expr[F[A] => F[B] => (A => B => C) => F[C]] =
      lambda3("fa", "fb", "f", app2(zipWithLambda[A, B, C](VarN(0, "f")), VarN(2, "fa"), VarN(1, "fb")))

    private def zipWithLambda[A, B, C](f: Expr[A => B => C]): Expr[F[A] => F[B] => F[C]] =
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
                      app2(shiftN(f, 7), varN[A]("aHead", 3), varN[B]("bHead", 1)),
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
