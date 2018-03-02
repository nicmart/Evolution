package evolution.algebra

import evolution.algebra.syntax.all._
import evolution.drawing.algebra.Weighted

trait DistributionEvolutionAlgebra[Evo[+ _]] extends EvolutionAlgebra[Evo] {
  self: NumericEvolutionAlgebra[Evo] =>
  implicit private lazy val alg: EvolutionAlgebra[Evo] = this

  def choose[A](as: IndexedSeq[A]): Evo[A] =
    int.map(i => as.apply(Math.abs(i) % as.length))

  def chooseEvo[A](evo1: Weighted[Evo[A]], evo2: Weighted[Evo[A]]): Evo[A] = {
    val totalWeight = evo1.weight + evo2.weight
    doubleBetween(0, totalWeight).flatMap { d =>
      if (d < evo1.weight) evo1.value.mapCons { case (a, tail) => cons(a, chooseEvo(evo1.map(_ => tail), evo2))}
      else evo2.value.mapCons { case (a, tail) => cons(a, chooseEvo(evo2.map(_ => tail), evo2))}
    }
  }
}
