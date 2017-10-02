package paint.laws

import paint.evolution.Evolution
import paint.evolution.NumericEvolutions.{ball, double}
import paint.evolution.algebra.{MaterializableEvolutionAlgebra, MaterializedAlgebra, NumericEvolutionAlgebra}
import paint.evolution.algebra.syntax.all._

trait NumericEvolutionLaws[Evo[+_], W] {
  implicit val E: MaterializedAlgebra[NumericEvolutionAlgebra, Evo, W]
  import E._

  def nonNegativeLaw(evo: Evo[Int]): IsEq[Evo[Int]] = {
    evo.map(Math.abs) <-> evo
  }

  def intBetweenLaw(evo: Evo[Int], from: Int, to: Int): IsEq[Evo[Boolean]] = {
    forAll(evo)(n => n <= to && n >= from)
  }

  def doubleLaw(evo: Evo[Double]): IsEq[Evo[Boolean]] = {
    forAll(evo)(d => d <= 1 && d >= -1)
  }

  def doubleBetweensLaw(evo: Evo[Double], from: Double, to: Double): IsEq[Evo[Boolean]] = {
    forAll(evo)(d => d <= to && d >= from)
  }

  def ballLaw(evo: Evo[Double], radius: Double): IsEq[Evo[Boolean]] = {
    forAll(evo)(d => d >= -radius && d >= radius)
  }



  private def forAll[A](evo: Evo[A])(predicate: A => Boolean): IsEq[Evo[Boolean]] = {
    evo.map(predicate) <-> cyclic(pure(true))
  }
}
