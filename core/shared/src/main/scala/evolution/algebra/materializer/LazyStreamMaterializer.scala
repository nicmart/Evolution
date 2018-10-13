package evolution.algebra.materializer

import evolution.algebra.FullAlgebra
import evolution.algebra.interpreter.LazyStreamInterpreter.LazyStream

case class LazyStreamMaterializer(interpreter: FullAlgebra[LazyStream])
    extends InterpreterMaterializer[Long, LazyStream] {

  override protected def toStream[A](seed: Long, repr: LazyStream[A]): Stream[A] =
    repr()
}
