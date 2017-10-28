package evolution.algebra

import evolution.algebra.syntax.all._
import evolution.geometry.Geometry.Point

trait PointEvolutionAlgebra[Evo[+ _]] extends EvolutionAlgebra[Evo] {
  self: NumericEvolutionAlgebra[Evo] with MotionEvolutionAlgebra[Evo] =>

  implicit private lazy val alg: NumericEvolutionAlgebra[Evo] with MotionEvolutionAlgebra[Evo] =
    this

  def cartesian(x: Evo[Double], y: Evo[Double]): Evo[Point] =
    x.zipWith(y)(Point.apply)

  def polar(norm: Evo[Double], angle: Evo[Double]): Evo[Point] =
    norm.zipWith(angle)(Point.polar)

  def uniformLinear(from: Point, speed: Point): Evo[Point] =
    solveIndependent(from)(constant(speed)).positional

  def uniformRadial(from: Point, radialSpeed: Double): Evo[Point] =
    solve0Static(from)(_.rotate(radialSpeed)).positional

  def rectangle2D(radius: Double): Evo[Point] =
    cartesian(ball(radius), ball(radius))

  def ball2D(radius: Double): Evo[Point] =
    polar(doubleBetween(0, radius), double.map(_ * 2 * Math.PI))

  def segment(p1: Point, p2: Point, speed: Double, lambda: Double = 0): Evo[Point] = {
    val norm = (p2 - p1).norm()
    if (lambda > 1) empty
    else {
      val nextLambda = lambda + speed / norm
      pure(p1 * lambda + p2 * (1 - lambda)).append(segment(p1, p2, speed, nextLambda))
    }
  }

  def ring(radius: Double): Evo[Point] =
    polar(
      constant(radius),
      double.map(_ * 2 * Math.PI)
    )

  def rotate(ev: Evo[Point])(center: Point, angle: Double): Evo[Point] =
    ev.map(p => (p - center).rotate(angle) + center)
}
