package paint.evolution.algebra.syntax

import paint.evolution.algebra.PointEvolutionAlgebra
import paint.evolution.algebra.syntax.all._
import paint.geometry.Geometry.Point

trait PointEvolutionSyntax {
  implicit final def pointSyntax[Evo[+ _]](evo: Evo[Point]): PointEvolutionOps[Evo] =
    new PointEvolutionOps(evo)
}

final class PointEvolutionOps[Evo[+ _]](val ev: Evo[Point]) extends AnyVal {
  def rotate(center: Point, angle: Double)(implicit E: PointEvolutionAlgebra[Evo]): Evo[Point] =
    ev.map(p => (p - center).rotate(angle) + center)
}