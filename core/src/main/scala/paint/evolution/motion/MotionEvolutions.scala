package paint.evolution.motion

import cats.kernel.Semigroup
import paint.evolution.Evolution
import paint.evolution.Evolution.transitionEvo
import cats.syntax.semigroup._

/**
  * Created by Nicolò Martini on 21/05/2017.
  */
object MotionEvolutions {

    def solve[A: Semigroup](a0: Position[A])(
        velocity: VelocityEvolution[A],
        p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
    ): Evolution[A] = {
        velocity.flatMapNext[A] { (vEq, evv2) =>
            val v1 = vEq(a0)
            val a1 = a0 |+| v1
            if (p(a1, v1)) a0 :: solve(a1)(evv2, p)
            else solve(a0)(evv2, p)
        }
    }

    def solveStatic[A: Semigroup](a0: Position[A])(
        velocityLaw: VelocityLaw[A],
        p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
    ): Evolution[A] =
        solve(a0)(staticVelocity(velocityLaw), p)

    def solveIndependentStatic[A: Semigroup](a0: Position[A])(
        velocity: Velocity[A],
        p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
    ): Evolution[A] = {
        solve[A](a0)(independentStaticVelocity(velocity), p)
    }

    def solveIndependent[A: Semigroup](a0: Position[A])(
        velocity: Evolution[Velocity[A]],
        p: FirstOrderPredicate[A] = trueFirstOrderPredicate[A]
    ): Evolution[A] = {
        solve[A](a0)(independentVelocity(velocity), p)
    }

    def solve2[A: Semigroup](a0: Position[A], v0: Velocity[A])(
        acceleration: AccelerationEvolution[A],
        p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
    ): Evolution[A] =
        acceleration flatMapNext { (accEq, evacc2) =>
            val acc1 = accEq(a0, v0)
            val v1 = acc1 |+| v0
            val a1 = a0 |+| v1
            if (p(a1, v1, acc1)) a0 :: solve2(a0 |+| v1, v1)(evacc2, p)
            else solve2(a0, v0)(evacc2, p)
        }

    def solve2Static[A: Semigroup](a0: Position[A], v0: Velocity[A])(
        acceleration: AccelerationLaw[A],
        p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
    ): Evolution[A] =
        solve2(a0, v0)(staticAcceleration[A](acceleration), p)

    def solve2IndependentStatic[A: Semigroup](a0: Position[A], v0: Velocity[A])(
        acceleration: Acceleration[A],
        p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
    ): Evolution[A] =
        solve2(a0, v0)(independentStaticAcceleration(acceleration), p)

    def solve2Independent[A: Semigroup](a0: Position[A], v0: Velocity[A])(
        acceleration: Evolution[Acceleration[A]],
        p: SecondOrderPredicate[A] = trueSecondOrderPredicate[A]
    ): Evolution[A] =
        solve2(a0, v0)(independentAcceleration(acceleration), p)
}
