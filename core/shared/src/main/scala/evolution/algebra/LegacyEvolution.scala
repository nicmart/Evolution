package evolution.algebra

trait LegacyEvolution[+A] {
  def run[Evo[+ _]](implicit alg: FullAlgebra[Evo]): Evo[A]
}
