package paint.evolution.algebra.impl

import paint.evolution.Repr
import paint.evolution.algebra.FullAlgebra
import paint.random.RNG

import scala.collection.immutable.Stream

final class RNGEvolutionAlgebra extends FullAlgebra[Repr] {

  override val empty: Repr[Nothing] =
    Repr { rng => (rng, None) }

  override def cons[A](head: A, tail: => Repr[A]): Repr[A] =
    Repr { rng => (rng, Some((head, tail))) }

  override def mapEmpty[A](eva: Repr[A])(eva2: => Repr[A]): Repr[A] = Repr[A] { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => eva2.run(rng2)
      case _ => (rng2, next)
    }
  }

  override def mapCons[A, B](eva: Repr[A])
    (f: (A, Repr[A]) => Repr[B]): Repr[B] = Repr[B] { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => (rng2, None)
      case Some((a, eva2)) => f(a, eva2).run(rng2)
    }
  }

  override val int: Repr[Int] = Repr { rng =>
    val (n, rng2) = rng.nextInt
    (rng2, Some((n, int)))
  }
}
