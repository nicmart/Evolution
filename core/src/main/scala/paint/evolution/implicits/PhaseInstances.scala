package paint.evolution.implicits

import paint.evolution.EvolutionLegacy
import paint.evolution.motion.{PhaseEvolutionOps, PhaseSpace}

/**
  * Created by Nicolò Martini on 24/05/2017.
  */
trait PhaseInstances {
  implicit def phaseEvolutionOps[A](ev: EvolutionLegacy[PhaseSpace[A]]): PhaseEvolutionOps[A] =
    PhaseEvolutionOps(ev)
}
