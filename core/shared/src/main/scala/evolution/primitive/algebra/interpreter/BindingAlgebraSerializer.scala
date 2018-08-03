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
    ctx => s"let($name, ${value(ctx)})(${expr(s"$$$name" :: ctx)})"

  override def fix[A](expr: CtxString[A]): CtxString[A] =
    ctx => s"fix(${expr(ctx)})"

  override def lambda[A, B](name: String, expr: CtxString[B]): CtxString[B] =
    ctx => s"$name -> ${expr(s"$$$name" :: ctx)}"

  override def app[A, B](f: CtxString[A], b: CtxString[B]): CtxString[A] =
    ctx => s"app(${f(ctx)}, ${b(ctx)})"
}
