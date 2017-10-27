package paint.evolution.algebra.interpreter

import paint.evolution.RNGRepr
import paint.evolution.algebra.FullAlgebra
import paint.random.RNG

import scala.collection.immutable.Stream

final class RNGEvolutionAlgebra extends FullAlgebra[RNGRepr] {

  override val empty: RNGRepr[Nothing] =
    RNGRepr { rng => (rng, None) }

  override def cons[A](head: A, tail: => RNGRepr[A]): RNGRepr[A] =
    RNGRepr { rng => (rng, Some((head, tail))) }

  override def mapEmpty[A](eva: RNGRepr[A])(eva2: => RNGRepr[A]): RNGRepr[A] = RNGRepr[A] { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => eva2.run(rng2)
      case _ => (rng2, next)
    }
  }

  override def mapCons[A, B](eva: RNGRepr[A])
    (f: (A, RNGRepr[A]) => RNGRepr[B]): RNGRepr[B] = RNGRepr[B] { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => (rng2, None)
      case Some((a, eva2)) => f(a, eva2).run(rng2)
    }
  }

  override val int: RNGRepr[Int] = RNGRepr { rng =>
    val (n, rng2) = rng.nextInt
    (rng2, Some((n, int)))
  }
}
