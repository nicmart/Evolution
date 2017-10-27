package paint.evolution.materializer

import paint.evolution.algebra.FullAlgebra
import paint.evolution.algebra.interpreter.LazyStreamInterpreter.LazyStream

case class StreamMaterializer(interpreter: FullAlgebra[Stream])
  extends InterpreterMaterializer[Long, Stream] {

  override protected def toStream[A](seed: Long, repr: Stream[A]): Stream[A] =
    repr
}
