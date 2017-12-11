package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra.{Drawing, DrawingAlgebra}
import evolution.geometry.Point

object Builder extends DrawingAlgebra[Drawing] {
  override def rnd(from: Double, to: Double): Drawing[Double] = new Drawing[Double] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Double] =
      alg.rnd(from, to)
  }

  override def const(x: Double): Drawing[Double] = new Drawing[Double] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Double] =
      alg.const(x)
  }

  override def cartesian(x: Drawing[Double], y: Drawing[Double]): Drawing[Point] = new Drawing[Point] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Point] =
      alg.cartesian(x.run(alg), y.run(alg))
  }

  override def polar(r: Drawing[Double], w: Drawing[Double]): Drawing[Point] = new Drawing[Point] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Point] =
      alg.polar(r.run(alg), w.run(alg))
  }

  override def integrateDouble(start: Double, f: Drawing[Double]): Drawing[Double] = new Drawing[Double] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Double] =
      alg.integrateDouble(start, f.run(alg))
  }

  override def integratePoint(start: Point, f: Drawing[Point]): Drawing[Point] = new Drawing[Point] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Point] =
      alg.integratePoint(start, f.run(alg))
  }

  override def deriveDouble(f: Drawing[Double]): Drawing[Double] = new Drawing[Double] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Double] =
      alg.deriveDouble(f.run(alg))
  }

  override def derivePoint(f: Drawing[Point]): Drawing[Point] = new Drawing[Point] {
    override def run[F[+ _]](alg: DrawingAlgebra[F]): F[Point] =
      alg.derivePoint(f.run(alg))
  }
}
