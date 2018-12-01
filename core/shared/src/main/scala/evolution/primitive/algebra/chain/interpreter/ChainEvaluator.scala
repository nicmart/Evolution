package evolution.primitive.algebra.chain.interpreter
import cats.Id
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.binding.interpreter.EvaluationResult._
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.Wip.ChainEvaluatorId

import scala.util.Random

object ChainEvaluator extends Chain[RNGRepr, EvaluationResult] {

  override def empty[A]: EvaluationResult[RNGRepr[A]] = Constant {
    debug(s"evaluating empty", RNGRepr { rng =>
      debug("running empty", (rng, None))
    })
  }

  override def cons[A](head: EvaluationResult[A], tail: EvaluationResult[RNGRepr[A]]): EvaluationResult[RNGRepr[A]] =
    Value(
      ctx => {
        debug("evaluating cons", RNGRepr { rng =>
          debug("running cons", (rng, Some((head.get(ctx), tail.get(ctx)))))
        })
      },
      s"cons($head, $tail)"
    )

  override def mapEmpty[A](
    eva: EvaluationResult[RNGRepr[A]],
    eva2: EvaluationResult[RNGRepr[A]]
  ): EvaluationResult[RNGRepr[A]] = Value { ctx =>
    debug(
      "evaluating mapEmpty",
      RNGRepr[A] { rng =>
        val (rng2, next) = eva.get(ctx).run(rng)
        debug("running mapEmpty", next match {
          case None => eva2.get(ctx).run(rng2)
          case _    => (rng2, next)
        })
      }
    )
  }

  override def mapCons[A, B](
    eva: EvaluationResult[RNGRepr[A]]
  )(f: EvaluationResult[A => RNGRepr[A] => RNGRepr[B]]): EvaluationResult[RNGRepr[B]] = Value(
    ctx => {
      debug(
        "evaluating mapCons",
        RNGRepr[B] { rng =>
          debug("running mapCons", {
            val (rng2, next) = eva.get(ctx).run(rng)
            next match {
              case None            => (rng2, None)
              case Some((a, eva2)) => f.get(ctx)(a)(eva2).run(rng2)
            }
          })
        }
      )
    },
    s"mapCons($eva)($f)"
  )
}

// TODO change name to Evaluator
// TODO EvaluationResult applicative instance makes the implementation stackoverflow
sealed abstract class ChainEvaluatorApplicative
    extends ChainApplicative[RNGRepr, Id, EvaluationResult](new ChainEvaluatorId)

object Wip {
  def apply(): Chain[RNGRepr, EvaluationResult] = new ChainEvaluatorApplicative {}

  private[chain] class ChainEvaluatorId extends Chain[RNGRepr, Id] {

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
        case _    => (rng2, next)
      }
    }

    override def mapCons[A, B](eva: RNGRepr[A])(f: A => RNGRepr[A] => RNGRepr[B]): RNGRepr[B] = RNGRepr[B] { rng =>
      val (rng2, next) = eva.run(rng)
      next match {
        case None            => (rng2, None)
        case Some((a, eva2)) => f(a)(eva2).run(rng2)
      }
    }
  }
}
