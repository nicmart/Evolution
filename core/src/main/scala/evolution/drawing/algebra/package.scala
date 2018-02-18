package evolution.drawing

import scala.language.higherKinds

package object algebra {
  type Empty[F[_, _]] = Unit
  type Drawing[E, A] = DrawingExpr[λ[F[_, _] => E], A]

  type EnvS[F[_, _], E[_[_, _]], In] = (F[E[F], In], E[F])
  type Env0[F[_, _]] = Empty[F]
  type Env1[F[_, _], In] = EnvS[F, Env0, In]
  type Env2[F[_, _], In1, In2] = EnvS[F, Env1[?[_, _], In2], In1]
  type Env3[F[_, _], In1, In2, In3] = EnvS[F, Env2[?[_, _], In2, In3], In1]

  type ExprS[E[_[_, _]], Out, In] = DrawingExpr[EnvS[?[_, _], E, In] , Out]
  type Expr0[Out] = DrawingExpr[Empty, Out]
  type Expr1[In, Out] = DrawingExpr[Env1[?[_, _], In], Out]
  type Expr2[In1, In2, Out] = DrawingExpr[Env2[?[_, _], In1, In2], Out]
}
