package evolution.primitive.algebra.derived
import evolution.geometry.Point
import evolution.primitive.algebra.evolution.Evolution

trait Derived[F[_], R[_]] {
  def cartesian(x: R[F[Double]], y: R[F[Double]]): R[F[Point]]
  def polar(radius: R[F[Double]], angle: R[F[Double]]): R[F[Point]]
  def constant[A](a: R[A]): R[F[A]]
}

class DefaultDerived[F[_], R[_]](alg: Evolution[F, R, String, String]) extends Derived[F, R] {
  import alg.bind._, alg.chain._, alg.constants._, alg.distribution._

  override def constant[A](a: R[A]): R[F[A]] =
    fix[F[A]](lambda("self", cons(a, varN("self", 0))))

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

  def flatMap[A, B](f: R[A => F[B]]): R[F[A] => F[B]] =
    ???

  def map[A, B](fa: R[F[A]], f: R[A => B]): R[F[B]] =
    ???

  def lambda2[A, B, C](var1: String, var2: String, expr: R[C]): R[A => B => C] =
    lambda[A, B => C](var1, lambda[B, C](var2, expr))

  def app2[A, B, C](f: R[A => B => C], a: R[A], b: R[B]): R[C] =
    app(app(f, a), b)

  def varN[A](name: String, n: Int): R[A] = if (n <= 0) var0 else shift(varN(name, n - 1))

  def zipWith[A, B, C](f: R[A => B => C]): R[F[A] => F[B] => F[C]] =
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

  def appZipWith[A, B, C](fa: R[F[A]], fb: R[F[B]], f: R[A => B => C]): R[F[C]] =
    app2(zipWith(lambda2[A, B, C]("fa", "fb", app2(f, varN("fx", 1), varN("fy", 2)))), fa, fb)
}
