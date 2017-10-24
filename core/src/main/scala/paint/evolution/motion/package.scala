package paint.evolution

import cats.kernel.Group
import cats.syntax.group._

import scala.language.implicitConversions

package object motion {
  type Position[A] = A
  type Velocity[A] = A
  type Acceleration[A] = A

  type PhaseSpace[A] = (Position[A], Velocity[A])

  type PositionLaw[A] = Position[A] => Position[A]
  type VelocityLaw[A] = Position[A] => Velocity[A]
  type AccelerationLaw[A] = (Position[A], Velocity[A]) => Acceleration[A]
  type SimpleAccelerationLaw[A] = Position[A] => Acceleration[A]

  type PositionEvolution[A] = EvolutionLegacy[PositionLaw[A]]
  type VelocityEvolution[A] = EvolutionLegacy[VelocityLaw[A]]
  type AccelerationEvolution[A] = EvolutionLegacy[AccelerationLaw[A]]
  type SimpleAccelerationEvolution[A] = EvolutionLegacy[SimpleAccelerationLaw[A]]

  type FirstOrderPredicate[A] = (Position[A], Velocity[A]) => Boolean
  type SecondOrderPredicate[A] = (Acceleration[A], Position[A], Velocity[A]) => Boolean

  def simpleToAcceleration[A](law: SimpleAccelerationLaw[A]): AccelerationLaw[A]
  = (position, _) => law(position)

  def simpleToAccelerationEvolution[A](eq: SimpleAccelerationEvolution[A]): AccelerationEvolution[A]
  = eq.map(simpleToAcceleration(_))

  def positionToVelocityEvolution[A: Group](posEv: PositionEvolution[A]): VelocityEvolution[A] =
    posEv.map(positionLaw => p => positionLaw(p) |-| p)

  def staticPosition[A](positionLaw: PositionLaw[A]): PositionEvolution[A] =
    EvolutionLegacy.constant(positionLaw)

  def independentStaticPosition[A](position: Position[A]): PositionEvolution[A] =
    EvolutionLegacy.constant(_ => position)

  def independentPosition[A](position: EvolutionLegacy[Position[A]]): PositionEvolution[A] =
    position.map(p => _ => p)

  def staticVelocity[A](velocityLaw: VelocityLaw[A]): VelocityEvolution[A] =
    EvolutionLegacy.constant(velocityLaw)

  def independentStaticVelocity[A](velocity: Velocity[A]): VelocityEvolution[A] =
    EvolutionLegacy.constant(_ => velocity)

  def independentVelocity[A](velocity: EvolutionLegacy[Velocity[A]]): VelocityEvolution[A] =
    velocity.map(v => _ => v)

  def staticAcceleration[A](accelerationLaw: AccelerationLaw[A]): AccelerationEvolution[A] =
    EvolutionLegacy.constant(accelerationLaw)

  def independentStaticAcceleration[A](acceleration: Acceleration[A]): AccelerationEvolution[A] =
    EvolutionLegacy.constant((_, _) => acceleration)

  def independentAcceleration[A](acceleration: EvolutionLegacy[Acceleration[A]]): AccelerationEvolution[A] =
    acceleration.map(acc => (_, _) => acc)

  def positionalFirstOrderPredicate[A](p: Position[A] => Boolean): FirstOrderPredicate[A] =
    (pos, _) => p(pos)

  def velocityFirstOrderPredicate[A](p: Velocity[A] => Boolean): FirstOrderPredicate[A] =
    (_, velocity) => p(velocity)

  def positionalSecondOrderPredicate[A](p: Position[A] => Boolean): SecondOrderPredicate[A] =
    (pos, _, _) => p(pos)

  def velocitySecondOrderPredicate[A](p: Velocity[A] => Boolean): SecondOrderPredicate[A] =
    (_, v, _) => p(v)

  def accelerationSecondOrderPredicate[A](p: Acceleration[A] => Boolean): SecondOrderPredicate[A] =
    (_, _, acc) => p(acc)

  def trueFirstOrderPredicate[A]: FirstOrderPredicate[A] = (_, _) => true
  def trueSecondOrderPredicate[A]: SecondOrderPredicate[A] = (_, _, _) => true
}
