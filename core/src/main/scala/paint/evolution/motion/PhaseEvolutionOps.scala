package paint.evolution.motion

import paint.evolution.EvolutionLegacy

/**
  * Created by Nicolò Martini on 24/05/2017.
  */
final case class PhaseEvolutionOps[A](ev: EvolutionLegacy[PhaseSpace[A]]) {
  def positional: EvolutionLegacy[A] = ev.map(_._1)
}
