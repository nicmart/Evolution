package evolution.compiler.phases.typing.model

import cats.Monad
import cats.mtl.FunctorRaise

trait TypeInference[M[_]] {
  def E: FunctorRaise[M, String]
  def S: Monad[M]
}

object TypeInference {

  def apply[M[_]](implicit ti: TypeInference[M]): TypeInference[M] = ti

  implicit def monad[M[_]](implicit ti: TypeInference[M]): Monad[M] = ti.S

  def instance[M[_]](implicit me: FunctorRaise[M, String], m: Monad[M]): TypeInference[M] =
    new TypeInference[M] {
      override def E: FunctorRaise[M, String] = me
      override def S: Monad[M] = m
    }

  object TypeInferenceInstances {
    implicit def monadInstance[M[_]](implicit M: TypeInference[M]): Monad[M] = M.S
    implicit def functorRaise[M[_]](implicit M: TypeInference[M]): FunctorRaise[M, String] = M.E
  }
}
