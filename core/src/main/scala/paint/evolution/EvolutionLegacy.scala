package paint.evolution

import paint.evolution.EvolutionLegacy._
import paint.evolution.algebra.Evolution
import paint.evolution.algebra.impl.RNGEvolutionAlgebra
import paint.random.{RNG, SimpleRNG}

import scala.collection.immutable.Stream
import scala.util.Random

final case class EvolutionLegacy[+A](run: RNG => (RNG, Option[(A, EvolutionLegacy[A])])) {

  // Primitives
  def flatMapNext[B](f: (A, EvolutionLegacy[A]) => EvolutionLegacy[B]): EvolutionLegacy[B] =
    EvolutionLegacy { rng =>
      val (rng2, next) = run(rng)
      next match {
        case None => (rng2, None)
        case Some((a, eva2)) => f(a, eva2).run(rng2)
      }
    }

  def append[B >: A](other: => EvolutionLegacy[B]): EvolutionLegacy[B] =
    EvolutionLegacy { rng =>
      val (rng2, next) = run(rng)
      next match {
        case None => other.run(rng2)
        case Some((a, eva2)) => (rng2, Some(a, eva2.append(other)))
      }
    }

  def unfold(from: RNG): Stream[A] = {
    val (rng2, next) = run(from)
    next match {
      case None => Stream.empty
      case Some((a, eva2)) => a #:: eva2.unfold(rng2)
    }
  }
}

object EvolutionLegacy {

  // Primitives
  def empty[A]: EvolutionLegacy[A] = EvolutionLegacy {
    (_, None)
  }

  def pure[A](value: A): EvolutionLegacy[A] =
    EvolutionLegacy {
      (_, Some(value, empty[A]))
    }
}
