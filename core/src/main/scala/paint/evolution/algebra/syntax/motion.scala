package paint.evolution.algebra.syntax

import cats.kernel.Group
import paint.evolution.algebra.MotionEvolutionAlgebra.PhaseSpace
import paint.evolution.algebra._
import paint.geometry.Geometry.Point

trait MotionEvolutionSyntax {
  implicit final def motionSyntax[Evo[+ _], A](evo: Evo[A]): MotionEvolutionOps[Evo, A] =
    new MotionEvolutionOps(evo)
  implicit final def phaseSpaceSyntax[Evo[+ _], A](evo: Evo[PhaseSpace[A]]): PhaseSpaceEvolutionOps[Evo, A] =
    new PhaseSpaceEvolutionOps(evo)
}

final class MotionEvolutionOps[Evo[+ _], A](val evo: Evo[A]) extends AnyVal {
  def toPhaseSpace(implicit E: MotionEvolutionAlgebra[Evo], group: Group[A]): Evo[PhaseSpace[A]] =
    E.toPhaseSpace(evo)
  def drawAlong(other: Evo[Point])(implicit E: MotionEvolutionAlgebra[Evo], ev: Evo[A] =:= Evo[PhaseSpace[Point]]): Evo[Point] =
    E.drawOnEvolution(evo, other)
}

final class PhaseSpaceEvolutionOps[Evo[+_], A](val evo: Evo[PhaseSpace[A]]) extends AnyVal {
  def positional[B](implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.map(evo)(_._1)
}