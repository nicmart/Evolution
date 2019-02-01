package evolution.primitive.algebra.distribution.interpreter

import cats.syntax.applicative._
import cats.implicits._
import evolution.algebra.representation.RNGRepr
import evolution.data.Evaluation
import evolution.data.Evaluation._
import evolution.primitive.algebra.distribution.Distribution

object DistributionEvaluator extends Distribution[RNGRepr, Evaluation] {
  override def uniform(
    fromEval: Evaluation[Double],
    toEval: Evaluation[Double]
  ): Evaluation[RNGRepr[Double]] =
    (fromEval, toEval) match {
      case (Constant(from, _), Constant(to, _)) =>
        Constant(uniformRepr(from, to), s"constant-uniform($fromEval, $toEval)")
      case _ =>
        Value(
          ctx => uniformRepr(fromEval.evaluateWith(ctx), toEval.evaluateWith(ctx)),
          s"non-constant-uniform($fromEval, $toEval)"
        )
    }

  override def uniformChoice[T](ts: List[Evaluation[T]]): Evaluation[RNGRepr[T]] =
    Value(ctx => uniformChoiceRepr(ts.map(_.evaluateWith(ctx))))

  override def uniformDiscrete(
    fromEval: Evaluation[Double],
    toEval: Evaluation[Double],
    stepEval: Evaluation[Double]): Evaluation[RNGRepr[Double]] =
    Value { ctx =>
      val (start, stop, step) = (fromEval.evaluateWith(ctx), toEval.evaluateWith(ctx), stepEval.evaluateWith(ctx))
      uniformChoiceRepr((start to stop by step).toList)
    }

  private def uniformRepr(from: Double, to: Double): RNGRepr[Double] = {
    lazy val self: RNGRepr[Double] = RNGRepr { rng =>
      val (n, rng2) = rng.nextInt
      val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
      val scaled = from + d * (to - from)
      (rng2, Some((scaled, self)))
    }
    self
  }

  private def uniformChoiceRepr[T](ts: List[T]): RNGRepr[T] =
    ts match {
      case Nil =>
        RNGRepr[T] { rng =>
          (rng, None)
        }
      case _ =>
        lazy val self: RNGRepr[T] = RNGRepr { rng =>
          val (n, rng2) = rng.nextInt
          val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
          val index = (d * ts.size).toInt
          (rng2, Some((ts(index), self)))
        }
        self
    }

}
