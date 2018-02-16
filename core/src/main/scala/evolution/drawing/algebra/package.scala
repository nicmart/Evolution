package evolution.drawing

import scala.language.higherKinds

package object algebra {
  type Empty[F[_, _]] = Unit
  type ConstE[E, F[_, _]] = E
  type SuccE[E[_[_, _]], In, F[_, _]] = (F[E[F], In], E[F])
  type Drawing[E, A] = DrawingE[λ[F[_, _] => E], A]
  type DrawingS[E[_[_, _]], Out, In] = DrawingE[λ[F[_, _] => (F[E[F], In], E[F])], Out]
}
