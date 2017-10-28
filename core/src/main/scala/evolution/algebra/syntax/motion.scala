package evolution.algebra.syntax

import cats.kernel.{Group, Semigroup}
import evolution.algebra.MotionEvolutionAlgebra.{AccelerationLaw, PhaseSpace}
import evolution.algebra._
import evolution.geometry.Point

trait MotionEvolutionSyntax {
  implicit final def motionSyntax[Evo[+ _], A](evo: Evo[A]): MotionEvolutionOps[Evo, A] =
    new MotionEvolutionOps(evo)
  implicit final def phaseSpaceSyntax[Evo[+ _], A](evo: Evo[PhaseSpace[A]]): PhaseSpaceEvolutionOps[Evo, A] =
    new PhaseSpaceEvolutionOps(evo)
  implicit final def accelerationLawSyntax[Evo[+ _], A](evo: Evo[AccelerationLaw[A]]): AccelerationLawEvolutionOps[Evo, A] =
    new AccelerationLawEvolutionOps(evo)
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

final class AccelerationLawEvolutionOps[Evo[+_], A](val evo: Evo[AccelerationLaw[A]]) extends AnyVal {
  def +(other: Evo[AccelerationLaw[A]])(
    implicit
    E: EvolutionAlgebra[Evo],
    sg: Semigroup[A]
  ): Evo[AccelerationLaw[A]] = {
    E.zipWith(evo, other){ (law1, law2) =>
      (position, velocity) => sg.combine(law1(position, velocity), law2(position, velocity))
    }
  }
}

