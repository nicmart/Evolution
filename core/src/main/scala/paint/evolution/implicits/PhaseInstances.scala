package paint.evolution.implicits

import paint.evolution.Evolution
import paint.evolution.motion.{PhaseEvolutionOps, PhaseSpace}

/**
  * Created by Nicolò Martini on 24/05/2017.
  */
trait PhaseInstances {
    implicit def phastToPositional[A](ev: Evolution[PhaseSpace[A]]): PhaseEvolutionOps[A] =
        PhaseEvolutionOps(ev)
}
