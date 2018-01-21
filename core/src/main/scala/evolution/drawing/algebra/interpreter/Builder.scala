package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra._
import evolution.geometry.Point

object Builder {
  def rnd[E[_[_, _]]](from: Double, to: Double): DrawingE[E, Double] = new DrawingE[E, Double] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], Double] =
      alg.rnd[E[F]](from, to)
  }

  def point[E[_[_, _]]](x: DrawingE[E, Double], y: DrawingE[E, Double]): DrawingE[E, Point] = new DrawingE[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], Point] =
      alg.point[E[F]](x.run(alg), y.run(alg))
  }

  def polar[E[_[_, _]]](r: DrawingE[E, Double], w: DrawingE[E, Double]): DrawingE[E, Point] = new DrawingE[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  def const[E[_[_, _]], T: DrawingAlgebra.Type](x: T): DrawingE[E, T] = new DrawingE[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.const(x)
  }
  def integrate[E[_[_, _]], T: DrawingAlgebra.Type](start: T, f: DrawingE[E, T]): DrawingE[E, T] = new DrawingE[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.integrate(start, f.run(alg))
  }
  def derive[E[_[_, _]], T: DrawingAlgebra.Type](f: DrawingE[E, T]): DrawingE[E, T] = new DrawingE[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.derive(f.run(alg))
  }

  def var0[E[_[_, _]], A]: DrawingS[E, A, A] = new DrawingE[Lambda[F[_, _] => SuccE[E, A, F]], A] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E[F], A], E[F]), A] =
      alg.var0[E[F], A]
  }
  def shift[E[_[_, _]], A, B](expr: DrawingE[E, A]): DrawingS[E, A, B] = new DrawingE[Lambda[F[_, _] => SuccE[E, B, F]], A] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E[F], B], E[F]), A] =
      alg.shift[E[F], A, B](expr.run(alg))
  }

  def let[E[_[_, _]], A, B](name: String, value: DrawingE[E, A])(expr: DrawingS[E, B, A]): DrawingE[E, B] = new DrawingE[E, B] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[E[F], B] =
      alg.let[E[F], A, B](name, value.run(alg))(expr.run(alg))
  }
}
