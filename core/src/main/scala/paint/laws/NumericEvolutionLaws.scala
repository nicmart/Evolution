package paint.laws

import paint.evolution.algebra.syntax.all._
import paint.evolution.algebra.NumericEvolutionAlgebra

trait NumericEvolutionLaws[Evo[+ _]] {
  implicit val E: NumericEvolutionAlgebra[Evo]

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
