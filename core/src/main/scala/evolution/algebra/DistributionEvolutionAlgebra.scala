package evolution.algebra

import evolution.algebra.syntax.all._

trait DistributionEvolutionAlgebra[Evo[+ _]] extends EvolutionAlgebra[Evo] {
  self: NumericEvolutionAlgebra[Evo] =>
  implicit private lazy val alg: EvolutionAlgebra[Evo] = this

  def choose[A](as: IndexedSeq[A]): Evo[A] =
    int.map(i => as.apply(Math.abs(i) % as.length))
}
