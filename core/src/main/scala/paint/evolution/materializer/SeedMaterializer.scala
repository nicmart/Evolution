package paint.evolution.materializer

import paint.evolution.SeedRepr
import paint.evolution.algebra.FullAlgebra

case class SeedMaterializer(interpreter: FullAlgebra[SeedRepr])
  extends InterpreterMaterializer[Long, SeedRepr] {

  override protected def toStream[A](seed: Long, repr: SeedRepr[A]): Stream[A] =
    repr.unfold(seed)
}
