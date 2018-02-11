package evolution.drawing.algebra

import cats.kernel.{Group, Semigroup}
import cats.implicits._
import evolution.drawing.algebra.TypeAlg.Pair
import evolution.geometry.Point

trait BindingAlgebra[F[-_, +_]] {
  def var0[E, A]: F[(F[E, A], E), A]
  def shift[E, A, B](expr: F[E, A]): F[(F[E, B], E), A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[(F[E, A], E), B]): F[E, B]
}

trait DrawingAlgebra[F[-_, +_]] extends BindingAlgebra[F] {
  def const[E, T: Type](x: T): F[E, T]
  def rnd[E](from: Double, to: Double): F[E, Double]
  def point[E](x: F[E, Double], y: F[E, Double]): F[E, Point]
  def polar[E](r: F[E, Double], w: F[E, Double]): F[E, Point]
  def integrate[E, T: Type](start: T, f: F[E, T]): F[E, T]
  def derive[E, T: Type](f: F[E, T]): F[E, T]
}

trait TypeAlg[F[_]] {
  def double: F[Double]
  def point: F[Point]
}

object TypeAlg {
  def apply[F[_]](doubleF: F[Double], pointF: F[Point]): TypeAlg[F] =
    new TypeAlg[F] {
      override def double: F[Double] = doubleF
      override def point: F[Point] = pointF
    }

  type Pair[F[_, _]] = TypeAlg[λ[X => TypeAlg[λ[Y => F[X, Y]]]]]
}

object PairAlg {
  def apply[F[_, _]](
    doubleDouble: F[Double, Double],
    doublePoint: F[Double, Point],
    pointDouble: F[Point, Double],
    pointPoint: F[Point, Point],
  ): Pair[F] =
    TypeAlg[λ[X => TypeAlg[F[X, ?]]]](
      TypeAlg[F[Double, ?]](doubleDouble, doublePoint),
      TypeAlg[F[Point, ?]](pointDouble, pointPoint)
    )
}

trait Type[T] {
  def run[F[_]](alg: TypeAlg[F]): F[T]
  def fold[F[_]](t: T)(f1: Double => F[Double], f2: Point => F[Point]): F[T] =
    run[λ[X => X => F[X]]](TypeAlg[λ[X => X => F[X]]](f1, f2))(t)
}

object Type {
  def apply[T](implicit t: Type[T]): Type[T] = t

  implicit val doubleType: Type[Double] = new Type[Double] {
    override def run[F[_]](alg: TypeAlg[F]): F[Double] = alg.double
  }
  implicit val pointType: Type[Point] = new Type[Point] {
    override def run[F[_]](alg: TypeAlg[F]): F[Point] = alg.point
  }

  def group[T](implicit t: Type[T]): Group[T] =
    t.run(TypeAlg(Group[Double], Group[Point]))
}

trait TypesPair[A, B] {
  def run[F[_, _]](alg: TypeAlg.Pair[F]): F[A, B]
}

object TypesPair {
  def get[A: Type, B: Type]: TypesPair[A, B] = new TypesPair[A, B] {
    def run[F[_, _]](alg: TypeAlg.Pair[F]): F[A, B] =
      Type[B].run[F[A, ?]](Type[A].run[λ[X => TypeAlg[λ[Y => F[X, Y]]]]](alg))
  }
}
