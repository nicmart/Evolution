package evolution.primitive.algebra.distribution.interpreter

import cats.Applicative
import cats.syntax.applicative._
import cats.implicits._
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.binding.interpreter.EvaluationResult
import evolution.primitive.algebra.distribution.Distribution

object DistributionEvaluator extends Distribution[RNGRepr, EvaluationResult] {
  override def uniform(
    fromEval: EvaluationResult[Double],
    toEval: EvaluationResult[Double]
  ): EvaluationResult[RNGRepr[Double]] =
    (fromEval, toEval).mapN { (from, to) =>
      lazy val self: RNGRepr[Double] = RNGRepr { rng =>
        val (n, rng2) = rng.nextInt
        val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
        val scaled = from + d * (to - from)
        (rng2, Some((scaled, self)))
      }
      self
    }
}
