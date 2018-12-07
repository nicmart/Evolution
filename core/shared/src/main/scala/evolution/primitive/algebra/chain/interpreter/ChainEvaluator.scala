package evolution.primitive.algebra.chain.interpreter

import evolution.algebra.representation.RNGRepr
import evolution.data.Evaluation
import evolution.data.Evaluation._
import evolution.primitive.algebra.chain.Chain

// TODO through applicative?
object ChainEvaluator extends Chain[RNGRepr, Evaluation] {

  override def empty[A]: Evaluation[RNGRepr[A]] = Constant {
    debug(s"evaluating empty", RNGRepr { rng =>
      debug("running empty", (rng, None))
    })
  }

  override def cons[A](evalHead: Evaluation[A], evalTail: Evaluation[RNGRepr[A]]): Evaluation[RNGRepr[A]] =
    Value(
      ctx => {
        val head = evalHead.evaluateWith(ctx)
        val tail = evalTail.evaluateWith(ctx)
        RNGRepr { rng =>
          debug(s"running cons($evalHead, $evalTail)", (rng, Some((head, tail))))
        }
      },
      s"cons($evalHead, $evalTail)"
    )

  override def mapEmpty[A](
    eva: Evaluation[RNGRepr[A]],
    eva2: Evaluation[RNGRepr[A]]
  ): Evaluation[RNGRepr[A]] = Value(
    ctx =>
      RNGRepr[A] { rng =>
        val (rng2, next) = eva.evaluateWith(ctx).run(rng)
        debug(s"running mapEmpty($eva, $eva2)", next match {
          case None => eva2.evaluateWith(ctx).run(rng2)
          case _    => (rng2, next)
        })
    },
    s"mapEmpty($eva, $eva2)"
  )

  override def mapCons[A, B](
    evalFA: Evaluation[RNGRepr[A]]
  )(evalF: Evaluation[A => RNGRepr[A] => RNGRepr[B]]): Evaluation[RNGRepr[B]] = Value(
    ctx => {
      val fa = evalFA.evaluateWith(ctx)
      val f = evalF.evaluateWith(ctx)
      RNGRepr[B] { rng =>
        debug(s"running mapCons($evalFA, $evalF)", {
          val (rng2, next) = fa.run(rng)
          next match {
            case None            => (rng2, None)
            case Some((a, eva2)) => f(a)(eva2).run(rng2)
          }
        })
      }
    },
    s"mapCons($evalFA)($evalF)"
  )
}
