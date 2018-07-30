package evolution.primitive.algebra

import _root_.evolution.algebra
import _root_.evolution.algebra.Evolution

package object interpreter {
  type Id[A] = A
  type ConstString[A] = String
  type CtxString[A] = List[String] => String
  type CtxEvolution[+A] = List[Any] => Evolution[A]
  type CtxScalar[A] = List[() => Any] => A
  type CtxF[F[_], A] = List[() => Any] => F[A]
}
