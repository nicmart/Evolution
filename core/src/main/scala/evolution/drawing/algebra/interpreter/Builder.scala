package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.{Drawing, DrawingAlgebra, DrawingOpen}
import evolution.geometry.Point

object Builder {
  def rnd[E](from: Double, to: Double): Drawing[E, Double] = new Drawing[E, Double] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, Double] =
      alg.rnd(from, to)
  }

  def point[E](x: Drawing[E, Double], y: Drawing[E, Double]): Drawing[E, Point] = new Drawing[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, Point] =
      alg.point(x.run(alg), y.run(alg))
  }

  def polar[E](r: Drawing[E, Double], w: Drawing[E, Double]): Drawing[E, Point] = new Drawing[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  def const[E, T: DrawingAlgebra.Type](x: T): Drawing[E, T] = new Drawing[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, T] =
      alg.const(x)
  }
  def integrate[E, T: DrawingAlgebra.Type](start: T, f: Drawing[E, T]): Drawing[E, T] = new Drawing[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, T] =
      alg.integrate(start, f.run(alg))
  }
  def derive[E, T: DrawingAlgebra.Type](f: Drawing[E, T]): Drawing[E, T] = new Drawing[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, T] =
      alg.derive(f.run(alg))
  }

  def var0[E, A]: DrawingOpen[E, A, A] = new DrawingOpen[E, A, A] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E, A], E), A] =
      alg.var0[E, A]
  }

  def shift[E, A, B](expr: Drawing[E, A]): DrawingOpen[E, B, A] = new DrawingOpen[E, B, A] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E, B], E), A] =
      alg.shift(expr.run(alg))
  }

  def let[E, A, B](name: String, value: Drawing[E, A])(expr: DrawingOpen[E, A, B]): Drawing[E, B] = new Drawing[E, B] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[E, B] =
      alg.let(name, value.run(alg))(expr.run(alg))
  }
}
