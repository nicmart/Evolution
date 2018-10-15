package evolution.algebra.syntax

import evolution.algebra.LegacyEvolutionAlgebra
import evolution.geometry.Point
import evolution.algebra.syntax.all._

trait PointEvolutionSyntax {
  implicit final def pointSyntax[Evo[+ _]](evo: Evo[Point]): PointEvolutionOps[Evo] =
    new PointEvolutionOps(evo)
}

final class PointEvolutionOps[Evo[+ _]](val ev: Evo[Point]) extends AnyVal {
  def rotate(center: Point, angle: Double)(implicit E: LegacyEvolutionAlgebra[Evo]): Evo[Point] =
    ev.map(p => (p - center).rotate(angle) + center)
}
