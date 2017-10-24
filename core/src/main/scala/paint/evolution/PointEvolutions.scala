package paint.evolution

import paint.evolution.EvolutionLegacy._
import paint.evolution.NumericEvolutions._
import paint.evolution.implicits._
import paint.evolution.motion.MotionEvolutions
import paint.geometry.Geometry.Point

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object PointEvolutions {
  def cartesian(x: EvolutionLegacy[Double], y: EvolutionLegacy[Double]): EvolutionLegacy[Point] =
    x.zipWith(y)(Point.apply)

  def polar(norm: EvolutionLegacy[Double], angle: EvolutionLegacy[Double]): EvolutionLegacy[Point] =
    norm.zipWith(angle)(Point.polar)

  // @TODO move into Semigroup evolutions
  def uniformLinear(from: Point, speed: Point): EvolutionLegacy[Point] =
    MotionEvolutions.solveIndependent(from)(constant(speed)).positional

  def uniformRadial(from: Point, radialSpeed: Double): EvolutionLegacy[Point] =
    MotionEvolutions.solve0Static(from)(_.rotate(radialSpeed)).positional

  def centeredIn(center: Point)(ev: EvolutionLegacy[Point]): EvolutionLegacy[Point] =
    ev.map(p => p + center)

  def rectangle2D(radius: Double): EvolutionLegacy[Point] =
    cartesian(ball(radius), ball(radius))

  def ball2D(radius: Double): EvolutionLegacy[Point] =
    polar(doubleRange(0, radius), double.map(_ * 2 * Math.PI))

  def segment(p1: Point, p2: Point, speed: Double, lambda: Double = 0): EvolutionLegacy[Point] = {
    val norm = (p2 - p1).norm()
    if (lambda > 1) empty
    else {
      val nextLambda = lambda + speed / norm
      pure(p1 * lambda + p2 * (1 - lambda)).append(segment(p1, p2, speed, nextLambda))
    }
  }

  def ring(radius: Double): EvolutionLegacy[Point] =
    polar(
      constant(radius),
      double.map(_ * 2 * Math.PI)
    )

  def rotate(center: Point, angle: Double, ev: EvolutionLegacy[Point]): EvolutionLegacy[Point] =
    ev.map(p => (p - center).rotate(angle) + center)
}
