package paint.evolution.algebra.interpreter

import paint.evolution.{RNGRepr, SeedRepr}
import paint.evolution.algebra.FullAlgebra
import paint.random.RNG

final class SeedInterpreter
  extends FullAlgebra[SeedRepr]
  with UnfoldInterpreter[Long, SeedRepr]
{

  override val empty: SeedRepr[Nothing] =
    SeedRepr { seed => (seed, None) }

  override def cons[A](head: A, tail: => SeedRepr[A]): SeedRepr[A] =
    SeedRepr { seed => (seed, Some((head, tail))) }

  override def mapEmpty[A](eva: SeedRepr[A])(eva2: => SeedRepr[A]): SeedRepr[A] = SeedRepr[A] { seed =>
    val (seed2, next) = eva.run(seed)
    next match {
      case None => eva2.run(seed2)
      case _ => (seed2, next)
    }
  }

  override def mapCons[A, B](eva: SeedRepr[A])
    (f: (A, SeedRepr[A]) => SeedRepr[B]): SeedRepr[B] = SeedRepr[B] { seed =>
    val (seed2, next) = eva.run(seed)
    next match {
      case None => (seed2, None)
      case Some((a, eva2)) => f(a, eva2).run(seed2)
    }
  }

  override val int: SeedRepr[Int] = SeedRepr { seed =>
    val (n, seed2) = RNG.next(seed)
    (seed2, Some((n, int)))
  }

  override def unfold[A](state: Long, repr: SeedRepr[A]): Stream[A] =
    repr.unfold(state)
}
