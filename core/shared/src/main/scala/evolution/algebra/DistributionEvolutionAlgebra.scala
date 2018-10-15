package evolution.algebra

import evolution.algebra.syntax.all._

trait DistributionEvolutionAlgebra[Evo[+ _]] extends LegacyEvolutionAlgebra[Evo] {
  self: NumericEvolutionAlgebra[Evo] =>
  implicit private lazy val alg: LegacyEvolutionAlgebra[Evo] = this

  def choose[A](as: IndexedSeq[A]): Evo[A] =
    int.map(i => as.apply(Math.abs(i) % as.length))

  def chooseBy[A](choiceEvo: Evo[Boolean], evo1: Evo[A], evo2: Evo[A]): Evo[A] =
    choiceEvo.mapCons {
      case (b, choiceEvo2) =>
        if (b) evo1.mapCons { case (a, evo12) => cons(a, chooseBy(choiceEvo2, evo12, evo2)) } else
          evo2.mapCons { case (a, evo22) => cons(a, chooseBy(choiceEvo2, evo1, evo22)) }
    }

  def dist(probability: Evo[Double], length1: Evo[Double], length2: Evo[Double]): Evo[Double] = {
    flatten(
      chooseBy(
        double.zipWith(probability) { case (d, p) => d < p },
        length1.map(l => seq(List.fill(Math.max(l.toInt, 0))(0.0))),
        length2.map(l => seq(List.fill(Math.max(l.toInt, 0))(1.0)))
      )
    )
  }
}
