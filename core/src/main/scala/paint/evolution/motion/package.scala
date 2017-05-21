package paint.evolution

import cats.kernel.Semigroup
import scala.language.implicitConversions

/**
  * Created by Nicolò Martini on 21/05/2017.
  */
package object motion {
    type Position[A] = A
    type Velocity[A] = A
    type Acceleration[A] = A

    type VelocityLaw[A] = Position[A] => Velocity[A]
    type AccelerationLaw[A] = (Position[A], Velocity[A]) => Acceleration[A]
    type SimpleAccelerationLaw[A] = Position[A] => Acceleration[A]

    type VelocityEvolution[A] = Evolution[VelocityLaw[A]]
    type AccelerationEvolution[A] = Evolution[AccelerationLaw[A]]
    type SimpleAccelerationEvolution[A] = Evolution[SimpleAccelerationLaw[A]]

    type FirstOrderPredicate[A] = (Position[A], Velocity[A]) => Boolean
    type SecondOrderPredicate[A] = (Acceleration[A], Position[A], Velocity[A]) => Boolean

    def simpleToAcceleration[A](law: SimpleAccelerationLaw[A]): AccelerationLaw[A]
        = (position, _) => law(position)

    def simpleToAccelerationEvolution[A](eq: SimpleAccelerationEvolution[A]): AccelerationEvolution[A]
        = eq.map(simpleToAcceleration(_))

    def staticVelocity[A](velocityLaw: VelocityLaw[A]): VelocityEvolution[A] =
        Evolution.pure(velocityLaw)

    def independentStaticVelocity[A](velocity: Velocity[A]): VelocityEvolution[A] =
        Evolution.pure(_ => velocity)

    def independentVelocity[A](velocity: Evolution[Velocity[A]]): VelocityEvolution[A] =
        velocity.map(v => _ => v)

    def staticAcceleration[A](accelerationLaw: AccelerationLaw[A]): AccelerationEvolution[A] =
        Evolution.pure(accelerationLaw)

    def independentStaticAcceleration[A](acceleration: Acceleration[A]): AccelerationEvolution[A] =
        Evolution.pure((_, _) => acceleration)

    def independentAcceleration[A](acceleration: Evolution[Acceleration[A]]): AccelerationEvolution[A] =
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
