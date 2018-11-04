package evolution.primitive
import cats.Monoid
import evolution.generator.Generator
import fastparse.noApi.Parser
import org.scalacheck.Gen

package object algebra {
  type ConstString[A] = String
  type CtxString[A] = List[String] => String
  type Ctx[A] = List[() => Any] => A
  type Composed[F[_], G[_], T] = F[G[T]]
  type GenRepr[R[_], T] = Int => Generator[R[T]]
  type Sized[R[_], T] = Int => R[T]
  type Const[A, B] = B
}
