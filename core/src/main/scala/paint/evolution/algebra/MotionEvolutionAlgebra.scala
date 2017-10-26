package paint.evolution.algebra

import cats.kernel.{Group, Semigroup}
import cats.syntax.group._
import paint.evolution.algebra.syntax.all._
import paint.geometry.Geometry.Point

trait MotionEvolutionAlgebra[Evo[+ _]] extends EvolutionAlgebra[Evo] {
  import MotionEvolutionAlgebra._
  private implicit lazy val E: EvolutionAlgebra[Evo] = this

  type PositionEvolution[A] = Evo[PositionLaw[A]]
  type VelocityEvolution[A] = Evo[VelocityLaw[A]]
  type AccelerationEvolution[A] = Evo[AccelerationLaw[A]]
  type SimpleAccelerationEvolution[A] = Evo[SimpleAccelerationLaw[A]]

  def solve0[A: Group](a0: Position[A])(
    position: PositionEvolution[A]
  ): Evo[PhaseSpace[A]] =
    solve(a0)(positionToVelocityEvolution(position))

  def solve0Static[A: Group](a0: Position[A])(
    positionLaw: PositionLaw[A]
  ): Evo[PhaseSpace[A]] =
    solve(a0)(positionToVelocityEvolution(staticPosition(positionLaw)))

  def solve0IndependentStatic[A: Group](a0: Position[A])(
    position: Position[A]
  ): Evo[PhaseSpace[A]] = {
    solve[A](a0)(positionToVelocityEvolution(independentStaticPosition(position)))
  }

  def solve0Independent[A: Group](a0: Position[A])(
    position: Evo[Position[A]]
  ): Evo[PhaseSpace[A]] = {
    solve[A](a0)(positionToVelocityEvolution(independentPosition(position)))
  }

  def solve[A: Semigroup](a0: Position[A])(
    velocity: VelocityEvolution[A]
  ): Evo[PhaseSpace[A]] = {
    (velocity: Evo[VelocityLaw[A]]).mapCons { (vEq, evv2) =>
      val v1 = vEq(a0)
      val a1 = a0 |+| v1
      (a0, v1) :: solve(a1)(evv2)
    }
  }

  def solveStatic[A: Semigroup](a0: Position[A])(
    velocityLaw: VelocityLaw[A]
  ): Evo[PhaseSpace[A]] =
    solve(a0)(staticVelocity(velocityLaw))

  def solveIndependentStatic[A: Semigroup](a0: Position[A])(
    velocity: Velocity[A]
  ): Evo[PhaseSpace[A]] = {
    solve[A](a0)(independentStaticVelocity(velocity))
  }

  def solveIndependent[A: Semigroup](a0: Position[A])(
    velocity: Evo[Velocity[A]]
  ): Evo[PhaseSpace[A]] = {
    solve[A](a0)(independentVelocity(velocity))
  }

  def solve2[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: AccelerationEvolution[A]
  ): Evo[PhaseSpace[A]] =
    (acceleration: Evo[AccelerationLaw[A]]).mapCons { (accEq, evacc2) =>
      val acc1 = accEq(a0, v0)
      val v1 = acc1 |+| v0
      val a1 = a0 |+| v1
      (a0, v0) :: solve2(a1, v1)(evacc2)
    }

  def solve2Static[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: AccelerationLaw[A]
  ): Evo[PhaseSpace[A]] =
    solve2(a0, v0)(staticAcceleration[A](acceleration))

  def solve2IndependentStatic[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: Acceleration[A]
  ): Evo[PhaseSpace[A]] =
    solve2(a0, v0)(independentStaticAcceleration(acceleration))

  def solve2Independent[A: Semigroup](a0: Position[A], v0: Velocity[A])(
    acceleration: Evo[Acceleration[A]]
  ): Evo[PhaseSpace[A]] =
    solve2(a0, v0)(independentAcceleration(acceleration))

  def toPhaseSpace[A: Group](evolution: Evo[A]): Evo[PhaseSpace[A]] = {
    evolution.slidingPair.map { case (a1, a2) => (a1, Group[A].remove(a2, a1)) }
  }

  // @TODO not the right place for this
  def drawOnEvolution(
    evphase: Evo[PhaseSpace[Point]],
    ev: Evo[Point]
  ): Evo[Point] = {
    evphase.zipWith[Point, Point](ev) { (phase, point) =>
      val (position, velocity) = phase
      position + point.rotate(velocity.angle)
    }
  }

  private def simpleToAcceleration[A](law: SimpleAccelerationLaw[A]): AccelerationLaw[A]
  = (position, _) => law(position)

  private def simpleToAccelerationEvolution[A](eq: SimpleAccelerationEvolution[A]): AccelerationEvolution[A]
  = E.map(eq)(simpleToAcceleration(_))

  private def positionToVelocityEvolution[A: Group](posEv: PositionEvolution[A]): VelocityEvolution[A] = {
    (posEv: Evo[PositionLaw[A]]).map(positionLaw => p => positionLaw(p) |-| p)
  }

  private def staticPosition[A](positionLaw: PositionLaw[A]): PositionEvolution[A] =
    constant(positionLaw)

  private def independentStaticPosition[A](position: Position[A]): PositionEvolution[A] =
    constant(_ => position)

  private def independentPosition[A](position: Evo[Position[A]]): PositionEvolution[A] =
    position.map(p => _ => p)

  private def staticVelocity[A](velocityLaw: VelocityLaw[A]): VelocityEvolution[A] =
    constant(velocityLaw)

  private def independentStaticVelocity[A](velocity: Velocity[A]): VelocityEvolution[A] =
    constant(_ => velocity)

  private def independentVelocity[A](velocity: Evo[Velocity[A]]): VelocityEvolution[A] =
    velocity.map(v => _ => v)

  private def staticAcceleration[A](accelerationLaw: AccelerationLaw[A]): AccelerationEvolution[A] =
    constant(accelerationLaw)

  private def independentStaticAcceleration[A](acceleration: Acceleration[A]): AccelerationEvolution[A] =
    constant((_, _) => acceleration)

  private def independentAcceleration[A](acceleration: Evo[Acceleration[A]]): AccelerationEvolution[A] =
    acceleration.map(acc => (_, _) => acc)
}

object MotionEvolutionAlgebra {
  type Position[A] = A
  type Velocity[A] = A
  type Acceleration[A] = A

  type PhaseSpace[A] = (Position[A], Velocity[A])

  type PositionLaw[A] = Position[A] => Position[A]
  type VelocityLaw[A] = Position[A] => Velocity[A]
  type AccelerationLaw[A] = (Position[A], Velocity[A]) => Acceleration[A]
  type SimpleAccelerationLaw[A] = Position[A] => Acceleration[A]
}
