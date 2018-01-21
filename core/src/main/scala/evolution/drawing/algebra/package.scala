package evolution.drawing

package object algebra {
  type Empty[F[_, _]] = Unit
  type ConstE[E, F[_, _]] = E
  type SuccE[E[_[_, _]], A, F[_, _]] = (F[E[F], A], E[F])
  type Drawing[E, A] = DrawingE[Lambda[F[_, _] => E], A]
  type DrawingS[E[_[_, _]], A, B] = DrawingE[Lambda[F[_, _] => SuccE[E, B, F]], A]
  def shift[A, E[_[_, _]]](drawing: DrawingE[E, A]): DrawingS[E, A, A] = ???
}
