package paint.evolution.implicits

import cats.kernel.Semigroup
import paint.evolution.motion.{AccelerationEvolution, AccelerationEvolutionOps}

/**
  * Created by Nicolò Martini on 31/05/2017.
  */
trait MotionEvolutionsInstances {
    implicit def accelerationEvolutionOps[A: Semigroup](ev: AccelerationEvolution[A]): AccelerationEvolutionOps[A] =
        AccelerationEvolutionOps(ev)
}
