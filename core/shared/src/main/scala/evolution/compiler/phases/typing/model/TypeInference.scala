package evolution.compiler.phases.typing.model

import cats.Monad
import cats.mtl.{ ApplicativeAsk, ApplicativeLocal, FunctorRaise, MonadState }
import evolution.compiler.types.TypeBindings

trait TypeInference[M[_]] {
  def E: FunctorRaise[M, String]
  def S: MonadState[M, TypeInference.State]
  def A: ApplicativeAsk[M, TypeBindings]
  def L: ApplicativeLocal[M, TypeBindings]
}

object TypeInference {
  case class State(vars: TypeVarGenerator)
  val empty = State(TypeVarGenerator.empty)

  def apply[M[_]](implicit ti: TypeInference[M]): TypeInference[M] = ti

  implicit def monad[M[_]](implicit ti: TypeInference[M]): Monad[M] = ti.S.monad

  def instance[M[_]](
    implicit
    me: FunctorRaise[M, String],
    ms: MonadState[M, TypeInference.State],
    aa: ApplicativeAsk[M, TypeBindings],
    al: ApplicativeLocal[M, TypeBindings]
  ): TypeInference[M] =
    new TypeInference[M] {
      override def E: FunctorRaise[M, String] = me
      override def S: MonadState[M, TypeInference.State] = ms
      override def A: ApplicativeAsk[M, TypeBindings] = aa
      override def L: ApplicativeLocal[M, TypeBindings] = al
    }

  object TypeInferenceInstances {
    implicit def monadInstance[M[_]](implicit M: TypeInference[M]): Monad[M] = M.S.monad
    implicit def functorRaise[M[_]](implicit M: TypeInference[M]): FunctorRaise[M, String] = M.E
    implicit def monadState[M[_]](implicit M: TypeInference[M]): MonadState[M, TypeInference.State] = M.S
    implicit def applicativeAsk[M[_]](implicit M: TypeInference[M]): ApplicativeAsk[M, TypeBindings] = M.A
    implicit def applicativeLocal[M[_]](implicit M: TypeInference[M]): ApplicativeLocal[M, TypeBindings] = M.L
  }
}
