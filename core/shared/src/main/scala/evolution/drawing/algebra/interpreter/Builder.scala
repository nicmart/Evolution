package evolution.drawing.algebra.interpreter

import cats.data.NonEmptyList
import evolution.drawing.algebra.{DrawingExpr, _}
import _root_.evolution.geometry.Point

object Builder {
  def add[T: Type](a: DrawingExpr[T], b: DrawingExpr[T]): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.add[T](a.run(alg), b.run(alg))
  }

  def inverse[T: Type](t: DrawingExpr[T]): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.inverse[T](t.run(alg))
  }

  def mul[T: Type](k: DrawingExpr[Double], t: DrawingExpr[T]): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.mul[T](k.run(alg), t.run(alg))
  }

  def rnd(from: DrawingExpr[Double], to: DrawingExpr[Double]): DrawingExpr[Double] = new DrawingExpr[Double] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[Double] =
      alg.rnd(from.run(alg), to.run(alg))
  }

  def point(x: DrawingExpr[Double], y: DrawingExpr[Double]): DrawingExpr[Point] = new DrawingExpr[Point] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[Point] =
      alg.point(x.run(alg), y.run(alg))
  }

  def polar(r: DrawingExpr[Double], w: DrawingExpr[Double]): DrawingExpr[Point] = new DrawingExpr[Point] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  def const[T: Type](x: T): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.const(x)
  }
  def integrate[T: Type](start: T, f: DrawingExpr[T]): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.integrate(start, f.run(alg))
  }
  def derive[T: Type](f: DrawingExpr[T]): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.derive(f.run(alg))
  }

  def slowDown[T: Type](by: DrawingExpr[Double], t: DrawingExpr[T]): DrawingExpr[T] = new DrawingExpr[T] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
      alg.slowDown[T](by.run(alg), t.run(alg))
  }

  def var0[A]: DrawingExpr[A] = new DrawingExpr[A] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[A] =
      alg.var0[A]
  }
  def shift[A](expr: DrawingExpr[A]): DrawingExpr[A] = new DrawingExpr[A] {
    override def run[F[_]](alg: DrawingAlgebra[F]): F[A] =
      alg.shift[A](expr.run(alg))
  }

  def let[In, Out](name: String, value: DrawingExpr[In])(expr: DrawingExpr[Out]): DrawingExpr[Out] =
    new DrawingExpr[Out] {
      override def run[F[_]](alg: DrawingAlgebra[F]): F[Out] =
        alg.let[In, Out](name, value.run(alg))(expr.run(alg))
    }

  def choose[T: Type](p: DrawingExpr[Double], drawing1: DrawingExpr[T], drawing2: DrawingExpr[T]): DrawingExpr[T] =
    new DrawingExpr[T] {
      override def run[F[_]](alg: DrawingAlgebra[F]): F[T] =
        alg.choose(p.run(alg), drawing1.run(alg), drawing2.run(alg))
    }

  def dist(
    weight: DrawingExpr[Double],
    length1: DrawingExpr[Double],
    length2: DrawingExpr[Double]
  ): DrawingExpr[Double] =
    new DrawingExpr[Double] {
      override def run[F[_]](alg: DrawingAlgebra[F]): F[Double] =
        alg.dist(weight.run(alg), length1.run(alg), length2.run(alg))
    }
}
