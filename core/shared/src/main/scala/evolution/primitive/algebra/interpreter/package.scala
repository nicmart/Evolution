package evolution.primitive.algebra

import _root_.evolution.algebra
import _root_.evolution.algebra.Evolution

package object interpreter {
  type Id[A] = A
  type ConstString[A] = String
  type CtxString[A] = List[String] => String

  // TODO: This is the lazy version
  // type Ctx[A] = List[() => Any] => () => A
  type Ctx[A] = List[() => Any] => A
}
