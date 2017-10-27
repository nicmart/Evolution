package paint.evolution.materializer

import paint.evolution.RNGRepr
import paint.random.RNG
import paint.evolution.algebra.FullAlgebra

case class RNGMaterializer(interpreter: FullAlgebra[RNGRepr])
  extends InterpreterMaterializer[Long, RNGRepr] {

  override protected def toStream[A](seed: Long, repr: RNGRepr[A]): Stream[A] =
    repr.unfold(RNG(seed))
}
