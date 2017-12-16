package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import evolution.geometry.Point

object Builder extends DrawingAlgebra[Drawing] {
  override def rnd(from: Double, to: Double): Drawing[Double] = new Drawing[Double] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Double] =
      alg.rnd(from, to)
  }

  override def cartesian(x: Drawing[Double], y: Drawing[Double]): Drawing[Point] = new Drawing[Point] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Point] =
      alg.cartesian(x.run(alg), y.run(alg))
  }

  override def polar(r: Drawing[Double], w: Drawing[Double]): Drawing[Point] = new Drawing[Point] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  override def const[T: DrawingAlgebra.Type](x: T): Drawing[T] = new Drawing[T] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[T] =
      alg.const(x)
  }
  override def integrate[T: DrawingAlgebra.Type](start: T, f: Drawing[T]): Drawing[T] = new Drawing[T] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[T] =
      alg.integrate(start, f.run(alg))
  }
  override def derive[T: DrawingAlgebra.Type](f: Drawing[T]): Drawing[T] = new Drawing[T] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[T] =
      alg.derive(f.run(alg))
  }
}
