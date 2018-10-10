package evolution.primitive
import evolution.generator.Generator
import fastparse.noApi.Parser
import org.scalacheck.Gen

package object algebra {
  type ConstString[A] = String
  type CtxString[A] = List[String] => String
  type ByVarParser[R[_], A] = List[String] => Parser[R[A]]
  type Ctx[A] = List[() => Any] => A
  type Composed[F[_], G[_], T] = F[G[T]]
  type GenRepr[R[_], T] = Int => Generator[R[T]]
}
