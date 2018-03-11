package evolution.drawing.algebra.interpreter

import cats.data.NonEmptyList
import evolution.drawing.algebra.{DrawingExpr, _}
import _root_.evolution.geometry.Point

final class Builder[E[_[_, _]]] {
  type NextBuilder[In] = Builder[EnvS[?[_, _], E, In]]
  type NextDrawingExpr[In, Out] = DrawingExpr[EnvS[?[_, _], E, In], Out]
  def withVar[In]: NextBuilder[In] = new NextBuilder[In]

  def add[T: Type](a: DrawingExpr[E, T], b: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.add[E[F], T](a.run(alg), b.run(alg))
  }

  def inverse[T: Type](t: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.inverse[E[F], T](t.run(alg))
  }

  def mul[T: Type](k: DrawingExpr[E, Double], t: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.mul[E[F], T](k.run(alg), t.run(alg))
  }

  def rnd(from: DrawingExpr[E, Double], to: DrawingExpr[E, Double]): DrawingExpr[E, Double] = new DrawingExpr[E, Double] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], Double] =
      alg.rnd[E[F]](from.run(alg), to.run(alg))
  }

  def point(x: DrawingExpr[E, Double], y: DrawingExpr[E, Double]): DrawingExpr[E, Point] = new DrawingExpr[E, Point] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], Point] =
      alg.point[E[F]](x.run(alg), y.run(alg))
  }

  def polar(r: DrawingExpr[E, Double], w: DrawingExpr[E, Double]): DrawingExpr[E, Point] = new DrawingExpr[E, Point] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  def const[T: Type](x: T): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.const(x)
  }
  def integrate[T: Type](start: T, f: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.integrate(start, f.run(alg))
  }
  def derive[T: Type](f: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.derive(f.run(alg))
  }

  def slowDown[T: Type](by: DrawingExpr[E, Double], t: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.slowDown[E[F], T](by.run(alg), t.run(alg))
  }

  def var0[A]: NextDrawingExpr[A, A] = new NextDrawingExpr[A, A] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[(F[E[F], A], E[F]), A] =
      alg.var0[E[F], A]
  }
  def shift[Out, In](expr: DrawingExpr[E, Out]): NextDrawingExpr[In, Out] = new NextDrawingExpr[In, Out] {
    override def run[F[_, _]](alg: DrawingAlgebra[F]): F[(F[E[F], In], E[F]), Out] =
      alg.shift[E[F], Out, In](expr.run(alg))
  }

  def let[In, Out](name: String, value: DrawingExpr[E, In])(expr: NextBuilder[In] => NextDrawingExpr[In, Out]): DrawingExpr[E, Out] =
    new DrawingExpr[E, Out] {
      override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], Out] =
        alg.let[E[F], In, Out](name, value.run(alg))(expr(withVar[In]).run(alg))
    }

  def choose[T: Type](p: DrawingExpr[E, Double], drawing1: DrawingExpr[E, T], drawing2: DrawingExpr[E, T]): DrawingExpr[E, T] =
    new DrawingExpr[E, T] {
      override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], T] =
        alg.choose(p.run(alg), drawing1.run(alg), drawing2.run(alg))
    }

  def dist(weight: DrawingExpr[E, Double], length1: DrawingExpr[E, Double], length2: DrawingExpr[E, Double]): DrawingExpr[E, Double] =
    new DrawingExpr[E, Double] {
      override def run[F[_, _]](alg: DrawingAlgebra[F]): F[E[F], Double] =
        alg.dist(weight.run(alg), length1.run(alg), length2.run(alg))
    }
}

object Builder {
  val start: Builder[Empty] = new Builder[Empty]
}
