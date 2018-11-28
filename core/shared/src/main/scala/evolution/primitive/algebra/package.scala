package evolution.primitive

package object algebra {
  type ConstString[A] = String
  type CtxString[A] = List[String] => String
  type Ctx[A] = List[() => Any] => A
  type Composed[F[_], G[_], T] = F[G[T]]
  type Sized[R[_], T] = Int => R[T]
  type Const[A, B] = B
}
