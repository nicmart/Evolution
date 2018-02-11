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

  def const[E[_[_, _]], T: Type](x: T): DrawingE[E, T] = new DrawingE[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.const(x)
  }
  def integrate[E[_[_, _]], T: Type](start: T, f: DrawingE[E, T]): DrawingE[E, T] = new DrawingE[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.integrate(start, f.run(alg))
  }
  def derive[E[_[_, _]], T: Type](f: DrawingE[E, T]): DrawingE[E, T] = new DrawingE[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.derive(f.run(alg))
  }

  def var0[E[_[_, _]], A]: DrawingS[E, A, A] = new DrawingE[λ[F[_, _] => SuccE[E, A, F]], A] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E[F], A], E[F]), A] =
      alg.var0[E[F], A]
  }
  def shift[E[_[_, _]], Out, In](expr: DrawingE[E, Out]): DrawingS[E, Out, In] = new DrawingE[λ[F[_, _] => SuccE[E, In, F]], Out] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E[F], In], E[F]), Out] =
      alg.shift[E[F], Out, In](expr.run(alg))
  }

  def let[E[_[_, _]], In, Out](name: String, value: DrawingE[E, In])(expr: DrawingS[E, Out, In]): DrawingE[E, Out] = new DrawingE[E, Out] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[E[F], Out] =
      alg.let[E[F], In, Out](name, value.run(alg))(expr.run(alg))
  }
}
