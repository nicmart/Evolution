package evolution.primitive.algebra.chain.interpreter
import evolution.algebra.representation.RNGRepr
import evolution.data.Result
import evolution.data.Result._
import evolution.primitive.algebra.chain.Chain

// TODO through applicative?
object ChainEvaluator extends Chain[RNGRepr, Result] {

  override def empty[A]: Result[RNGRepr[A]] = Constant {
    debug(s"evaluating empty", RNGRepr { rng =>
      debug("running empty", (rng, None))
    })
  }

  override def cons[A](evalHead: Result[A], evalTail: Result[RNGRepr[A]]): Result[RNGRepr[A]] =
    Value(
      ctx => {
        val head = evalHead.evaluate(ctx)
        val tail = evalTail.evaluate(ctx)
        RNGRepr { rng =>
          debug(s"running cons($evalHead, $evalTail)", (rng, Some((head, tail))))
        }
      },
      s"cons($evalHead, $evalTail)"
    )

  override def mapEmpty[A](
    eva: Result[RNGRepr[A]],
    eva2: Result[RNGRepr[A]]
  ): Result[RNGRepr[A]] = Value(
    ctx =>
      RNGRepr[A] { rng =>
        val (rng2, next) = eva.evaluate(ctx).run(rng)
        debug(s"running mapEmpty($eva, $eva2)", next match {
          case None => eva2.evaluate(ctx).run(rng2)
          case _    => (rng2, next)
        })
    },
    s"mapEmpty($eva, $eva2)"
  )

  override def mapCons[A, B](
    evalFA: Result[RNGRepr[A]]
  )(evalF: Result[A => RNGRepr[A] => RNGRepr[B]]): Result[RNGRepr[B]] = Value(
    ctx => {
      val fa = evalFA.evaluate(ctx)
      val f = evalF.evaluate(ctx)
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
