package paint.laws

import paint.evolution.`new`.Evolutions

trait EvolutionLaws[Evolution[_], W] {
  implicit val E: Evolutions[Evolution, W]

  import E._

  def covariantComposition[A, B, C](ev: Evolution[A], f: A => B, g: B => C): IsEq[Evolution[C]] =
    ev.map(f).map(g) <-> ev.map(f andThen g)
}
