package evolution.primitive
import fastparse.noApi.Parser

package object algebra {
  type ConstString[A] = String
  type CtxString[A] = List[String] => String
  type ByVarParser[R[_], A] = List[String] => Parser[R[A]]
  type Ctx[A] = List[() => Any] => A
  type Composed[F[_], G[_], T] = F[G[T]]
}
