package evolution.algebra.materializer

import evolution.random.RNG
import evolution.algebra.FullAlgebra
import evolution.algebra.representation.RNGRepr

case class RNGMaterializer(interpreter: FullAlgebra[RNGRepr])
  extends InterpreterMaterializer[Long, RNGRepr] {

  override protected def toStream[A](seed: Long, repr: RNGRepr[A]): Stream[A] =
    repr.unfold(RNG(seed))
}
