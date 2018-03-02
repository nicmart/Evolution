package evolution.algebra.interpreter

import evolution.algebra.FullAlgebra
import evolution.algebra.representation.RNGRepr
import evolution.drawing.algebra.Weighted
import evolution.random.RNG

final class RNGInterpreter extends FullAlgebra[RNGRepr] {
  /**
    * Primitives
    */
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

  override lazy val double: RNGRepr[Double] = RNGRepr { rng =>
    val (n, rng2) = rng.nextInt
    val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
    (rng2, Some((d, double)))
  }

  /**
    * Optimisations
    */
  override def concat[A](
    evo1: RNGRepr[A],
    evo2: => RNGRepr[A]
  ): RNGRepr[A] = RNGRepr { rng =>
    val (rng2, next) = evo1.run(rng)
    next match {
      case None => evo2.run(rng2)
      case Some((a, evo12)) => (rng2, Some((a, concat(evo12, evo2))))
    }
  }

  override def flatMap[A, B](eva: RNGRepr[A])(f: (A) => RNGRepr[B]): RNGRepr[B] = RNGRepr { rng =>
    val (rng2, next)  = eva.run(rng)
    next match {
      case None => (rng2, None)
      case Some((a, eva2)) =>
        val (rng3, next2) = f(a).run(rng2)
        next2 match {
          case None => flatMap(eva2)(f).run(rng3)
          case Some((b, nextb)) => (rng3, Some((b, concat(nextb, flatMap(eva2)(f)))))
        }
    }
  }

  override def zipWith[A, B, C](eva: RNGRepr[A], evb: RNGRepr[B])
    (f: (A, B) => C): RNGRepr[C] =
    RNGRepr { rng =>
      val (rng2, nexta) = eva.run(rng)
      val (rng3, nextb) = evb.run(rng2)
      (nexta, nextb) match {
        case (Some((a, eva2)), Some((b, evb2))) => (rng3, Some((f(a, b), zipWith(eva2, evb2)(f))))
        case _ => (rng3, None)
      }
    }

  override def map[A, B](eva: RNGRepr[A])(f: (A) => B): RNGRepr[B] = RNGRepr { rng =>
    val (rng2, next) = eva.run(rng)
    next match {
      case None => (rng2, None)
      case Some((a, eva2)) => (rng2, Some((f(a), map(eva2)(f))))
    }
  }

  override def chooseEvo[A](evo1: Weighted[RNGRepr[A]], evo2: Weighted[RNGRepr[A]]): RNGRepr[A] = {
    val totalWeight = evo1.weight + evo2.weight
    val cut = (evo1.weight / totalWeight) * (Int.MaxValue.toDouble - Int.MinValue) + Int.MinValue
    RNGRepr { rng =>
      val (n, rng2) = rng.nextInt
      if (n < cut) {
        val (rng3, nextOpt) = evo1.value.run(rng2)
        (rng3, nextOpt.map { case (a, next) => (a, chooseEvo(evo1.map(_ => next), evo2)) })
      }
      else {
        val (rng3, nextOpt) = evo2.value.run(rng2)
        (rng3, nextOpt.map { case (a, next) => (a, chooseEvo(evo1, evo2.map(_ => next))) })
      }
    }
  }
}
