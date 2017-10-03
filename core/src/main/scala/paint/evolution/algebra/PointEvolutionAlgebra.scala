package paint.evolution.algebra

import paint.evolution.Evolution
import paint.evolution.algebra.syntax.all._
import paint.geometry.Geometry.Point

trait PointEvolutionAlgebra[Evo[+_]]
  extends NumericEvolutionAlgebra[Evo]
{
  implicit private val alg: NumericEvolutionAlgebra[Evo] = this

  def cartesian(x: Evo[Double], y: Evo[Double]): Evo[Point] =
    x.zipWith(y)(Point.apply)

  def polar(norm: Evo[Double], angle: Evo[Double]): Evo[Point] =
    norm.zipWith(angle)(Point.polar)
}
