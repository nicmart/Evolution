package paint.evolution.algebra

import paint.evolution.algebra.syntax.all._

trait DistributionEvolutionAlgebra[Evo[+ _]] extends NumericEvolutionAlgebra[Evo] {
  implicit private val alg: NumericEvolutionAlgebra[Evo] = this

  def choose[A](as: IndexedSeq[A]): Evo[A] =
    int.map(i => as.apply(Math.abs(i) % as.length))
}
