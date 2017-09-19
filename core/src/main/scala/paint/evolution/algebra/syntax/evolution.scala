package paint.evolution.algebra.syntax

import paint.evolution.algebra.{EvolutionAlgebra, EvolutionMaterialization}

trait EvolutionSyntax {
  implicit final def syntaxEvolution[Evo[_], A](evo: Evo[A]): EvolutionOps[Evo, A] =
    new EvolutionOps(evo)
  implicit final def syntaxMaterializableEvolution[Evo[_], W, A](evo: Evo[A]): MaterializableEvolutionOps[Evo, W, A] =
    new MaterializableEvolutionOps(evo)
}

final class EvolutionOps[Evo[_], A](val ev: Evo[A]) extends AnyVal {
  def flatMap[B](f: A => Evo[B])(implicit E: EvolutionAlgebra[Evo]): Evo[B] =
    E.flatMap(ev)(f)
  def map[B](f: A => B)(implicit E: EvolutionAlgebra[Evo]): Evo[B] =
    E.map(ev)(f)
  def append(otherEv: => Evo[A])(implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.concat(ev, otherEv)
  def take(n: Int)(implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.take(ev, n)
  def drop(n: Int)(implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.drop(ev, n)
  def zipWith[B, C](evb: Evo[B])(f: (A, B) => C)(implicit E: EvolutionAlgebra[Evo]): Evo[C] =
    E.zipWith(ev, evb)(f)
  def scan[Z](z: Z)(f: (Z, A) => Z)(implicit E: EvolutionAlgebra[Evo]): Evo[Z] =
    E.scan(ev)(z)(f)
  def repeat(times: Int)(implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.repeat(ev, times)
  def slowDown(n: Int)(implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.slowDown(ev, n)
  def slowDownBy(evn: Evo[Int])(implicit E: EvolutionAlgebra[Evo]): Evo[A] =
    E.slowDownBy(ev, evn)
}

final class MaterializableEvolutionOps[Evo[_], W, A](val ev: Evo[A]) extends AnyVal {
  def run(w: W)(implicit materialization: EvolutionMaterialization[Evo, W]): Stream[A] =
    materialization.run(ev, w)
}