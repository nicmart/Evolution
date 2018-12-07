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
      case (Constant(from, _), Constant(to, _)) => Constant(repr(from, to), s"constant-uniform($fromEval, $toEval)")
      case _ =>
        Value(
          ctx => repr(fromEval.evaluateWith(ctx), toEval.evaluateWith(ctx)),
          s"non-constant-uniform($fromEval, $toEval)")
    }

  private def repr(from: Double, to: Double): RNGRepr[Double] = {
    lazy val self: RNGRepr[Double] = RNGRepr { rng =>
      val (n, rng2) = rng.nextInt
      val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
      val scaled = from + d * (to - from)
      (rng2, Some((scaled, self)))
    }
    self
  }
}
