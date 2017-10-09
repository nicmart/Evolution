package paint.evolution.motion

import cats.kernel.Semigroup
import cats.syntax.semigroup._

final case class AccelerationEvolutionOps[A: Semigroup](evo: AccelerationEvolution[A]) {
  def sum(other: AccelerationEvolution[A]): AccelerationEvolution[A] = {
    evo.zipWith(other) { (law1, law2) =>
      (pos, vel) => law1(pos, vel) |+| law2(pos, vel)
    }
  }

  def +(other: AccelerationEvolution[A]): AccelerationEvolution[A] = sum(other)
}
