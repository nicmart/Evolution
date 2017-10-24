package paint.evolution.algebra

trait Evolution[A] {
  def run[Evo[+_]](implicit alg: FullAlgebra[Evo]): Evo[A]
}