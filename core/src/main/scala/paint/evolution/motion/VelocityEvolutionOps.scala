package paint.evolution.motion

import cats.kernel.Semigroup
import cats.syntax.semigroup._

/**
  * Created by Nicolò Martini on 31/05/2017.
  */
final case class VelocityEvolutionOps[A: Semigroup](evo: VelocityEvolution[A]) {
  def sum(other: VelocityEvolution[A]): VelocityEvolution[A] = {
    evo.zipWith(other) { (law1, law2) =>
      pos => law1(pos) |+| law2(pos)
    }
  }

  def +(other: VelocityEvolution[A]): VelocityEvolution[A] = sum(other)
}
