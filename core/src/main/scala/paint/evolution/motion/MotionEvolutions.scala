package paint.evolution.motion

import cats.kernel.{Group, Semigroup}
import cats.syntax.semigroup._
import paint.evolution.EvolutionLegacy
import paint.geometry.Geometry.Point
//import cats.syntax.group._

object MotionEvolutions {
  def solve0[A: Group](a0: Position[A])(
    position: PositionEvolution[A],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    solve(a0)(positionToVelocityEvolution(position), p)

  def solve0Static[A: Group](a0: Position[A])(
    positionLaw: PositionLaw[A],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    solve(a0)(positionToVelocityEvolution(staticPosition(positionLaw)), p)

  def solve0IndependentStatic[A: Group](a0: Position[A])(
    position: Position[A],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] = {
    solve[A](a0)(positionToVelocityEvolution(independentStaticPosition(position)), p)
  }

  def solve0Independent[A: Group](a0: Position[A])(
    position: EvolutionLegacy[Position[A]],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] = {
    solve[A](a0)(positionToVelocityEvolution(independentPosition(position)), p)
  }

  def solve[A: Semigroup](a0: Position[A])(
    velocity: VelocityEvolution[A],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] = {
    velocity.flatMapNext { (vEq, evv2) =>
      val v1 = vEq(a0)
      val a1 = a0 |+| v1
      if (p(a1, v1)) (a0, v1) :: solve(a1)(evv2, p)
      else solve(a0)(evv2, p)
    }
  }

  def solveStatic[A: Semigroup](a0: Position[A])(
    velocityLaw: VelocityLaw[A],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    solve(a0)(staticVelocity(velocityLaw), p)

  def solveIndependentStatic[A: Semigroup](a0: Position[A])(
    velocity: Velocity[A],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] = {
    solve[A](a0)(independentStaticVelocity(velocity), p)
  }

  def solveIndependent[A: Semigroup](a0: Position[A])(
    velocity: EvolutionLegacy[Velocity[A]],
    p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] = {
    solve[A](a0)(independentVelocity(velocity), p)
  }

  def solve2[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: AccelerationEvolution[A],
    p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    acceleration flatMapNext { (accEq, evacc2) =>
      val acc1 = accEq(a0, v0)
      val v1 = acc1 |+| v0
      val a1 = a0 |+| v1
      if (p(a1, v1, acc1)) (a0, v0) :: solve2(a1, v1)(evacc2, p)
      else solve2(a0, v0)(evacc2, p)
    }

  def solve2Static[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: AccelerationLaw[A],
    p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    solve2(a0, v0)(staticAcceleration[A](acceleration), p)

  def solve2IndependentStatic[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: Acceleration[A],
    p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    solve2(a0, v0)(independentStaticAcceleration(acceleration), p)

  def solve2Independent[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: EvolutionLegacy[Acceleration[A]],
    p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
  ): EvolutionLegacy[PhaseSpace[A]] =
    solve2(a0, v0)(independentAcceleration(acceleration), p)

  def toPhaseSpace[A: Group](evolution: EvolutionLegacy[A]): EvolutionLegacy[PhaseSpace[A]] = {
    evolution.slidingPairs.map { case (a1, a2) => (a1, Group[A].remove(a2, a1)) }
  }

  // @TODO not the right place for this
  def drawOnEvolution(
    evphase: EvolutionLegacy[PhaseSpace[Point]],
    ev: EvolutionLegacy[Point]
  ): EvolutionLegacy[Point] = {
    evphase.zipWith[Point, Point](ev) { (phase, point) =>
      val (position, velocity) = phase
      position + point.rotate(velocity.angle)
    }
  }
}
