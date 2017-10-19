package paint.evolution.algebra.syntax

import cats.kernel.Group
import paint.evolution.algebra.MotionEvolutionAlgebra.PhaseSpace
import paint.evolution.algebra._
import paint.geometry.Geometry.Point

trait MotionEvolutionSyntax {
  implicit final def motionSyntax[Evo[+ _], A](evo: Evo[A]): MotionEvolutionOps[Evo, A] =
    new MotionEvolutionOps(evo)
}

final class MotionEvolutionOps[Evo[+ _], A](val evo: Evo[A]) extends AnyVal {
  def toPhaseSpace(implicit E: MotionEvolutionAlgebra[Evo], group: Group[A]): Evo[PhaseSpace[A]] =
    E.toPhaseSpace(evo)
  def drawAlong(other: Evo[Point])(implicit E: MotionEvolutionAlgebra[Evo], ev: Evo[A] =:= Evo[PhaseSpace[Point]]): Evo[Point] =
    E.drawOnEvolution(evo, other)
  def positional(
    implicit
    E: EvolutionAlgebra[Evo],
    ev: Evo[A] =:= Evo[PhaseSpace[Point]]
  ): Evo[Point] =
    E.map[PhaseSpace[Point], Point](evo)(_._1)
}