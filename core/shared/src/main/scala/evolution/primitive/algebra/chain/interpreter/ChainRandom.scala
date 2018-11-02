package evolution.primitive.algebra.chain.interpreter
import cats.Id
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainRandom.ChainRandomId

sealed abstract class ChainRandom extends ChainApplicative[Id, RNGRepr, Id, EvaluationResult](new ChainRandomId)

object ChainRandom {
  def apply: Chain[Id, RNGRepr, EvaluationResult] = new ChainRandom {}

  private[chain] class ChainRandomId extends Chain[Id, RNGRepr, Id] {

    override def empty[A]: RNGRepr[A] = RNGRepr { rng =>
      (rng, None)
    }

    override def cons[A](head: A, tail: RNGRepr[A]): RNGRepr[A] = RNGRepr { rng =>
      (rng, Some((head, tail)))
    }

    override def mapEmpty[A](eva: RNGRepr[A], eva2: RNGRepr[A]): RNGRepr[A] = RNGRepr[A] { rng =>
      val (rng2, next) = eva.run(rng)
      next match {
        case None => eva2.run(rng2)
        case _ => (rng2, next)
      }
    }

    override def mapCons[A, B](eva: RNGRepr[A])(f: A => RNGRepr[A] => RNGRepr[B]): RNGRepr[B] = RNGRepr[B] { rng =>
      val (rng2, next) = eva.run(rng)
      next match {
        case None => (rng2, None)
        case Some((a, eva2)) => f(a)(eva2).run(rng2)
      }
    }
  }
}
