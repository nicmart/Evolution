package evolution.primitive.algebra.constants.interpreter
import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.typeclass.VectorSpace

class ConstantsExpr[F[_]] extends Constants[Expr[F, ?]] {

  override def int(n: Int): Expr[F, Int] =
    new Expr[F, Int] {
      override def run[R[_]](alg: Evolution[F, R]): R[Int] =
        alg.constants.int(n)
    }

  override def double(d: Double): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.double(d)
    }

  override def point(x: Expr[F, Double], y: Expr[F, Double]): Expr[F, Point] =
    new Expr[F, Point] {
      override def run[R[_]](alg: Evolution[F, R]): R[Point] =
        alg.constants.point(x.run(alg), y.run(alg))
    }

  override def x(point: Expr[F, Point]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.x(point.run(alg))
    }

  override def y(point: Expr[F, Point]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.y(point.run(alg))
    }

  override def add[T: Semigroup](a: Expr[F, T], b: Expr[F, T]): Expr[F, T] =
    new Expr[F, T] {
      override def run[R[_]](alg: Evolution[F, R]): R[T] =
        alg.constants.add(a.run(alg), b.run(alg))
    }

  override def div(a: Expr[F, Double], b: Expr[F, Double]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.div(a.run(alg), b.run(alg))
    }

  override def exp(a: Expr[F, Double], b: Expr[F, Double]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.exp(a.run(alg), b.run(alg))
    }

  override def inverse[T: Group](a: Expr[F, T]): Expr[F, T] =
    new Expr[F, T] {
      override def run[R[_]](alg: Evolution[F, R]): R[T] =
        alg.constants.inverse(a.run(alg))
    }

  override def sin(d: Expr[F, Double]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.sin(d.run(alg))
    }

  override def cos(d: Expr[F, Double]): Expr[F, Double] =
    new Expr[F, Double] {
      override def run[R[_]](alg: Evolution[F, R]): R[Double] =
        alg.constants.cos(d.run(alg))
    }

  override def multiply[T: VectorSpace](k: Expr[F, Double], t: Expr[F, T]): Expr[F, T] =
    new Expr[F, T] {
      override def run[R[_]](alg: Evolution[F, R]): R[T] =
        alg.constants.multiply(k.run(alg), t.run(alg))
    }

  override def eq[T: Eq](a: Expr[F, T], b: Expr[F, T]): Expr[F, Boolean] =
    new Expr[F, Boolean] {
      override def run[R[_]](alg: Evolution[F, R]): R[Boolean] =
        alg.constants.eq(a.run(alg), b.run(alg))
    }

  override def ifThen[T](condition: Expr[F, Boolean], a: Expr[F, T], b: Expr[F, T]): Expr[F, T] =
    new Expr[F, T] {
      override def run[R[_]](alg: Evolution[F, R]): R[T] =
        alg.constants.ifThen(condition.run(alg), a.run(alg), b.run(alg))
    }
}
