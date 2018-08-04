package evolution.primitive.algebra

import _root_.evolution.algebra
import _root_.evolution.algebra.Evolution

package object interpreter {
  type Id[A] = A
  type ConstString[A] = String
  type CtxString[A] = List[String] => String
  type Ctx[A] = List[() => Any] => A
}
