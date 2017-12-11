package evolution.drawing.algebra

import evolution.geometry.Point

trait DrawingAlgebra[F[+_]] {
  def rnd(from: Double, to: Double): F[Double]
  def const(x: Double): F[Double]
  def cartesian(x: F[Double], y: F[Double]): F[Point]
  def polar(r: F[Double], w: F[Double]): F[Point]
  def integrateDouble(start: Double, f: F[Double]): F[Double]
  def integratePoint(start: Point, f: F[Point]): F[Point]
  def deriveDouble(f: F[Double]): F[Double]
  def derivePoint(f: F[Point]): F[Point]
}
