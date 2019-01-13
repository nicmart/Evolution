package evolution.primitive.algebra.derived
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution
import evolution.typeclass.VectorSpace
import evolution.typeclass.VectorSpace._
import cats.implicits._

trait Derived[F[_], R[_]] {
  def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]]
  def polar(radius: R[F[Double]], angle: R[F[Double]]): R[F[Point]]
  def constant[A](a: R[A]): R[F[A]]
  def integrate[A: VectorSpace](start: R[A], speed: R[F[A]]): R[F[A]]
  def concat[A](fa1: R[F[A]], fa2: R[F[A]]): R[F[A]]
  def map[A, B](fa: R[F[A]], f: R[A => B]): R[F[B]]
  def flatMap[A, B](fa: R[F[A]], f: R[A => F[B]]): R[F[B]]
  def take[T](n: R[Int], ft: R[F[T]]): R[F[T]]
}

class DefaultDerived[F[_], R[_]](alg: Evolution[F, R]) extends Derived[F, R] {
  import alg.bind._, alg.chain._, alg.constants._, alg.distribution._

  override def constant[A](a: R[A]): R[F[A]] =
    fix[F[A]](lambda("self", cons(shift(a), varN("self", 0))))

  override def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]] =
    app2(zipWith(lambda2[Double, Double, Point]("fx", "fy", point(varN("fx", 1), varN("fy", 0)))), x, y)

  override def polar(radius: R[F[Double]], angle: R[F[Double]]): R[F[Point]] =
    app2(
      zipWith(
        lambda2[Double, Double, Point](
          "radius",
          "angle",
          multiply(varN("radius", 1), point(cos(varN("angle", 0)), sin(varN("angle", 0)))))),
      radius,
      angle)

  override def concat[A](fa1: R[F[A]], fa2: R[F[A]]): R[F[A]] =
    app2(concatLambda, fa1, fa2)

  override def integrate[A: VectorSpace](start: R[A], speed: R[F[A]]): R[F[A]] =
    app2(integrateLambda[A], start, speed)

  override def map[A, B](fa: R[F[A]], f: R[A => B]): R[F[B]] =
    app(mapLambda(f), fa)

  override def flatMap[A, B](fa: R[F[A]], f: R[A => F[B]]): R[F[B]] =
    app(flatMapLambda(f), fa)

  private def mapLambda[A, B](f: R[A => B]): R[F[A] => F[B]] =
    fix[F[A] => F[B]](
      lambda(
        "self",
        lambda[F[A], F[B]](
          "fa",
          mapCons(varN[F[A]]("fa", 0))(
            lambda2(
              "head",
              "tail",
              cons(app(f, varN[A]("head", 1)), app(varN[F[A] => F[B]]("self", 3), varN[F[A]]("tail", 0)))
            )
          )
        )
      )
    )

  private def flatMapLambda[A, B](f: R[A => F[B]]): R[F[A] => F[B]] =
    fix[F[A] => F[B]](
      lambda(
        "self",
        lambda(
          "fa",
          mapCons[A, B](varN[F[A]]("fa", 0))(
            lambda2[A, F[A], F[B]](
              "head",
              "tail",
              concat(
                app(f, varN[A]("head", 1)),
                app(varN[F[A] => F[B]]("self", 3), varN[F[A]]("tail", 0))
              )
            )
          )
        )
      )
    )

  private def concatLambda[A]: R[F[A] => F[A] => F[A]] =
    fix[F[A] => F[A] => F[A]](
      lambda(
        "self",
        lambda2[F[A], F[A], F[A]](
          "fa1",
          "fa2",
          mapEmpty[A](
            mapCons(varN[F[A]]("fa1", 1))(
              lambda2(
                "head1",
                "tail1",
                cons(
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
    fix[T => F[T] => F[T]](
      lambda(
        "self",
        lambda2[T, F[T], F[T]](
          "start",
          "speed",
          mapCons(varN[F[T]]("speed", 0))(
            lambda2[T, F[T], F[T]](
              "speedHead",
              "speedTail",
              cons(
                varN("start", 3),
                app2[T, F[T], F[T]](
                  varN("self", 4),
                  add(varN[T]("start", 3), varN[T]("speedHead", 1)),
                  varN[F[T]]("speedTail", 0)))
            )
          )
        )
      ))

  override def take[T](n: R[Int], ft: R[F[T]]): R[F[T]] =
    app2(takeLambda, n, ft)

  private def takeLambda[T]: R[Int => F[T] => F[T]] =
    fix[Int => F[T] => F[T]](
      lambda(
        "self",
        lambda(
          "n",
          lambda(
            "ft",
            ifThen(
              alg.constants.eq(int(0), varN[Int]("n", 1)),
              empty[T],
              mapCons(varN[F[T]]("ft", 0))(
                lambda(
                  "head",
                  lambda(
                    "tail",
                    cons(
                      varN[T]("head", 1),
                      app2(
                        varN[Int => F[T] => F[T]]("self", 4),
                        add(varN[Int]("n", 3), inverse(int(1))),
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
    lambda[A, B => C](var1, lambda[B, C](var2, expr))

  private def lambda3[A, B, C, D](var1: String, var2: String, var3: String, expr: R[D]): R[A => B => C => D] =
    lambda[A, B => C => D](var1, lambda2[B, C, D](var2, var3, expr))

  private def app2[A, B, C](f: R[A => B => C], a: R[A], b: R[B]): R[C] =
    app(app(f, a), b)

  private def varN[A](name: String, n: Int): R[A] = if (n <= 0) var0(name) else shift(varN(name, n - 1))

  private def zipWith[A, B, C](f: R[A => B => C]): R[F[A] => F[B] => F[C]] =
    fix[F[A] => F[B] => F[C]](
      lambda[F[A] => F[B] => F[C], F[A] => F[B] => F[C]](
        "self",
        lambda2[F[A], F[B], F[C]](
          "fa",
          "fb",
          mapCons(varN[F[A]]("fa", 1))(lambda2(
            "aHead",
            "aTail",
            mapCons(varN[F[B]]("fb", 2))(lambda2(
              "bHead",
              "bTail",
              cons(
                app2(f, varN[A]("aHead", 3), varN[B]("bHead", 1)),
                app2(varN[F[A] => F[B] => F[C]]("self", 6), varN[F[A]]("aTail", 2), varN[F[B]]("bTail", 0)))
            ))
          ))
        )
      )
    )
}
