package paint.evolution.materializer

import paint.evolution.algebra.{Evolution, FullAlgebra}

trait InterpreterMaterializer[S, Repr[+_]] extends Materializer[S] {
  override def materialize[A](seed: S, evo: Evolution[A]): Stream[A] =
    toStream(seed, evo.run(interpreter))

  protected def interpreter: FullAlgebra[Repr]
  protected def toStream[A](seed: S, repr: Repr[A]): Stream[A]
}
