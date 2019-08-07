package evolution.compiler.phases.typing

import cats.Monad
import cats.implicits._
import cats.mtl.{ ApplicativeAsk, ApplicativeLocal, FunctorRaise, MonadState }
import evolution.compiler.ast.AST.Identifier
import evolution.compiler.types.{ Type, TypeBinding }
import evolution.compiler.types.TypeClasses.Qualified
import evolution.language.Typer.{ BindingContextOps, TypeContext, TypeVars }

trait TypeInference[M[_]] {
  def E: FunctorRaise[M, String]
  def S: MonadState[M, TypeInference.State]
  def A: ApplicativeAsk[M, TypeContext]
  def L: ApplicativeLocal[M, TypeContext]
}

object TypeInference {
  case class State(vars: TypeVars)
  val empty = State(TypeVars.empty)

  def apply[M[_]](implicit ti: TypeInference[M]): TypeInference[M] = ti

  implicit def monad[M[_]](implicit ti: TypeInference[M]): Monad[M] = ti.S.monad

  def newTypeVar[M[_]](implicit TI: TypeInference[M]): M[Qualified[Type]] = for {
    state <- TI.S.get
    qt = Qualified[Type](state.vars.current)
    _ <- TI.S.set(state.copy(vars = state.vars.next))
  } yield qt

  def getType[M[_]](name: String)(implicit TI: TypeInference[M]): M[Identifier] =
    TI.A.ask.flatMap(_.getBinding(name))

  def withVarType[M[_], T](name: String, qt: Qualified[Type])(t: M[T])(implicit TI: TypeInference[M]): M[T] =
    for {
      t <- TI.L.local(_.updated(name, TypeBinding.Variable(name, qt)))(t)
    } yield t

  def instance[M[_]](
    implicit
    me: FunctorRaise[M, String],
    ms: MonadState[M, TypeInference.State],
    aa: ApplicativeAsk[M, TypeContext],
    al: ApplicativeLocal[M, TypeContext]
  ): TypeInference[M] =
    new TypeInference[M] {
      override def E: FunctorRaise[M, String] = me
      override def S: MonadState[M, TypeInference.State] = ms
      override def A: ApplicativeAsk[M, TypeContext] = aa
      override def L: ApplicativeLocal[M, TypeContext] = al
    }

  object TypeInferenceInstances {
    implicit def monadInstance[M[_]](implicit M: TypeInference[M]): Monad[M] = M.S.monad
    implicit def functorRaise[M[_]](implicit M: TypeInference[M]): FunctorRaise[M, String] = M.E
    implicit def monadState[M[_]](implicit M: TypeInference[M]): MonadState[M, TypeInference.State] = M.S
    implicit def applicativeAsk[M[_]](implicit M: TypeInference[M]): ApplicativeAsk[M, TypeContext] = M.A
    implicit def applicativeLocal[M[_]](implicit M: TypeInference[M]): ApplicativeLocal[M, TypeContext] = M.L
  }
}