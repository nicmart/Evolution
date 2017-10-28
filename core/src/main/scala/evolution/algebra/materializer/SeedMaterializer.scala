package evolution.algebra.materializer

import evolution.algebra.FullAlgebra
import evolution.algebra.representation.SeedRepr

case class SeedMaterializer(interpreter: FullAlgebra[SeedRepr])
  extends InterpreterMaterializer[Long, SeedRepr] {

  override protected def toStream[A](seed: Long, repr: SeedRepr[A]): Stream[A] =
    repr.unfold(seed)
}
