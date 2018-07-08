package evolution.drawing.algebra

import cats.data.NonEmptyList
import cats.kernel.{Group, Semigroup}
import cats.implicits._
import _root_.evolution.geometry.Point
import TypeAlg.Pair

trait BindingAlgebra[F[_, _]] {
  def var0[E, A]: F[(F[E, A], E), A]
  def shift[E, A, B](expr: F[E, A]): F[(F[E, B], E), A]
  def let[E, A, B](name: String, value: F[E, A])(expr: F[(F[E, A], E), B]): F[E, B]
}

trait DrawingAlgebra[F[_, _]] extends BindingAlgebra[F] {
  def const[E, T: Type](x: T): F[E, T]
  def mul[E, T: Type](k: F[E, Double], t: F[E, T]): F[E, T]
  def add[E, T: Type](a: F[E, T], b: F[E, T]): F[E, T]
  def inverse[E, T: Type](a: F[E, T]): F[E, T]
  def rnd[E](from: F[E, Double], to: F[E, Double]): F[E, Double]
  def point[E](x: F[E, Double], y: F[E, Double]): F[E, Point]
  def polar[E](r: F[E, Double], w: F[E, Double]): F[E, Point]
  def integrate[E, T: Type](start: T, f: F[E, T]): F[E, T]
  def derive[E, T: Type](f: F[E, T]): F[E, T]
  def slowDown[E, T: Type](by: F[E, Double], drawing: F[E, T]): F[E, T]

  /**
    * 1. Draw a double `d` from `dist`
    * 2. If `d` < 0.5 draw a T from `drawing1`
    * 3. Draw a T from `drawing2` otherwise
    */
  def choose[E, T: Type](dist: F[E, Double], drawing1: F[E, T], drawing2: F[E, T]): F[E, T]

  /**
    * Build an evolution of 0.0s or 1.0s defined in this way:
    * 1. draw a probability `p` from `probability`
    * 2. With probability `p` choose `length1`, `length2` otherwise
    * 3. If `length1` was chosen, draw `l` from `length1` and return `l`s 0.0s
    * 4. If `length2` was chosen, draw `l` from `length2` and return `l`s 1.0s
    * 5. GOTO 1
    *
    * The originating idea was to be able to use `choose` for example to alternate `drawing1` and `drawing2` every
    * 100 iterations
    */
  def dist[E](probability: F[E, Double], length1: F[E, Double], length2: F[E, Double]): F[E, Double]
}

trait DrawingExpr[E[_[_, _]], A] {
  def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], A]
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
