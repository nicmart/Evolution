package paint.evolution.`new`

trait EvolutionMaterialization[Evo[_], W] {
  def run[A](evo: Evo[A], world: W): Stream[A]
}
