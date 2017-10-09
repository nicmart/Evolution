package paint.evolution.motion

import paint.evolution.Evolution

/**
  * Created by Nicolò Martini on 24/05/2017.
  */
final case class PhaseEvolutionOps[A](ev: Evolution[PhaseSpace[A]]) {
  def positional: Evolution[A] = ev.map(_._1)
}
