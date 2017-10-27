package paint.evolution.materializer

import paint.evolution.SeedRepr
import paint.evolution.algebra.FullAlgebra
import paint.evolution.algebra.interpreter.LazyStreamInterpreter.LazyStream

case class LazyStreamMaterializer(interpreter: FullAlgebra[LazyStream])
  extends InterpreterMaterializer[Long, LazyStream] {

  override protected def toStream[A](seed: Long, repr: LazyStream[A]): Stream[A] =
    repr()
}
