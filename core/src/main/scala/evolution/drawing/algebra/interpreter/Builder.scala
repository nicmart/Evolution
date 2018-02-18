package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra._
import evolution.geometry.Point

object Builder {
  def rnd[E[_[_, _]]](from: Double, to: Double): DrawingExpr[E, Double] = new DrawingExpr[E, Double] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], Double] =
      alg.rnd[E[F]](from, to)
  }

  def point[E[_[_, _]]](x: DrawingExpr[E, Double], y: DrawingExpr[E, Double]): DrawingExpr[E, Point] = new DrawingExpr[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], Point] =
      alg.point[E[F]](x.run(alg), y.run(alg))
  }

  def polar[E[_[_, _]]](r: DrawingExpr[E, Double], w: DrawingExpr[E, Double]): DrawingExpr[E, Point] = new DrawingExpr[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  def const[E[_[_, _]], T: Type](x: T): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.const(x)
  }
  def integrate[E[_[_, _]], T: Type](start: T, f: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.integrate(start, f.run(alg))
  }
  def derive[E[_[_, _]], T: Type](f: DrawingExpr[E, T]): DrawingExpr[E, T] = new DrawingExpr[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E[F], T] =
      alg.derive(f.run(alg))
  }

  def var0[E[_[_, _]], A]: ExprS[E, A, A] = new DrawingExpr[EnvS[?[_, _], E, A], A] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E[F], A], E[F]), A] =
      alg.var0[E[F], A]
  }
  def shift[E[_[_, _]], Out, In](expr: DrawingExpr[E, Out]): ExprS[E, Out, In] = new DrawingExpr[EnvS[?[_, _], E, In], Out] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[(F[E[F], In], E[F]), Out] =
      alg.shift[E[F], Out, In](expr.run(alg))
  }

  def let[E[_[_, _]], In, Out](name: String, value: DrawingExpr[E, In])(expr: ExprS[E, Out, In]): DrawingExpr[E, Out] = new DrawingExpr[E, Out] {
    override def run[F[- _, + _]](alg: DrawingAlgebra[F]): F[E[F], Out] =
      alg.let[E[F], In, Out](name, value.run(alg))(expr.run(alg))
  }
}
