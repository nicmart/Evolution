package evolution.primitive.algebra

import cats.data.NonEmptyList
import cats.kernel.{Group, Semigroup}
import cats.implicits._
import _root_.evolution.geometry.Point
import TypeAlg.Pair

trait BindingAlgebra[F[_]] {
  def var0[A]: F[A]
  def shift[A](expr: F[A]): F[A]
  def let[A, B](name: String, value: F[A])(expr: F[B]): F[B]
  def fix[A](expr: F[A]): F[A]
}

trait CoreDrawingAlgebra[S[_], F[_]] {
  def empty[A]: F[A]
  def cons[A](head: S[A], tail: F[A]): F[A]
  def mapEmpty[A](eva: F[A])(eva2: F[A]): F[A]
  def mapCons[A, B](eva: F[A])(f: F[B]): F[B]
}

trait ScalarAlgebra[S[_]] {
  def double(d: Double): S[Double]
  def point(p: Point): S[Point]
}

trait DrawingAlgebra[S[_], F[_]] {
  val drawing: CoreDrawingAlgebra[S, F]
  val scalar: ScalarAlgebra[S]
  val bindS: BindingAlgebra[S]
  val bindF: BindingAlgebra[F]
}

trait DrawingExpr[A] {
  def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): F[A]
}

trait ScalarExpr[A] {
  def run[S[_], F[_]](alg: DrawingAlgebra[S, F]): S[A]
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
  def foldF[F[_], G[_]](t: F[T])(f1: F[Double] => G[Double], f2: F[Point] => G[Point]): G[T] =
    run[λ[X => F[X] => G[X]]](TypeAlg[λ[X => F[X] => G[X]]](f1, f2))(t)
}

object Type {
  def apply[T](implicit t: Type[T]): Type[T] = t

  implicit val doubleType: Type[Double] = new Type[Double] {
    override def run[F[_]](alg: TypeAlg[F]): F[Double] = alg.double
  }
  implicit val pointType: Type[Point] = new Type[Point] {
    override def run[F[_]](alg: TypeAlg[F]): F[Point] = alg.point
  }

  implicit def group[T](implicit t: Type[T], groupDouble: Group[Double], groupPoint: Group[Point]): Group[T] =
    t.run(TypeAlg(groupDouble, groupPoint))
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
