package evolution.primitive.algebra.interpreter

import evolution.primitive.algebra.BindingAlgebra

object BindingAlgebraSerializer extends BindingAlgebra[CtxString] {
  override def var0[A]: CtxString[A] = {
    case head :: tail => head
  }
  override def shift[A](expr: CtxString[A]): CtxString[A] = {
    case head :: tail => expr(tail)
  }
  override def let[A, B](name: String, value: CtxString[A])(expr: CtxString[B]): CtxString[B] =
    ctx => s"let($name, ${value(ctx)}, ${expr(s"$$$name" :: ctx)})"

  override def fix[A](expr: CtxString[A]): CtxString[A] =
    ctx => s"fix(self -> ${expr("$self" :: ctx)})"
}
