package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import evolution.geometry.Point

object Builder extends DrawingAlgebra[Drawing] {
  override def rnd[E](from: Double, to: Double): Drawing[E, Double] = new Drawing[E, Double] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, Double] =
      alg.rnd(from, to)
  }

  override def point[E](x: Drawing[E, Double], y: Drawing[E, Double]): Drawing[E, Point] = new Drawing[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, Point] =
      alg.point(x.run(alg), y.run(alg))
  }

  override def polar[E](r: Drawing[E, Double], w: Drawing[E, Double]): Drawing[E, Point] = new Drawing[E, Point] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  override def const[E, T: DrawingAlgebra.Type](x: T): Drawing[E, T] = new Drawing[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, T] =
      alg.const(x)
  }
  override def integrate[E, T: DrawingAlgebra.Type](start: T, f: Drawing[E, T]): Drawing[E, T] = new Drawing[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, T] =
      alg.integrate(start, f.run(alg))
  }
  override def derive[E, T: DrawingAlgebra.Type](f: Drawing[E, T]): Drawing[E, T] = new Drawing[E, T] {
    override def run[F[-_, +_]](alg: DrawingAlgebra[F]): F[E, T] =
      alg.derive(f.run(alg))
  }
}
