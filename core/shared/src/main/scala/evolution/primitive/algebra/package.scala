package evolution.primitive

package object algebra {
  type CtxString[A] = List[String] => String
  type Composed[F[_], G[_], T] = F[G[T]]
}
