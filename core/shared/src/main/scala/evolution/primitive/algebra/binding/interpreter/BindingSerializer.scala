package evolution.primitive.algebra.binding.interpreter
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.binding.Binding

// TODO missing test
object BindingSerializer extends Binding[CtxString, String] {
  override def var0[A]: CtxString[A] = {
    case head :: tail => head
  }
  override def shift[A](expr: CtxString[A]): CtxString[A] = {
    case head :: tail => expr(tail)
  }
  override def let[A, B](name: String, value: CtxString[A], expr: CtxString[B]): CtxString[B] =
    ctx => s"let($name, ${value(ctx)}, ${expr(s"$$$name" :: ctx)})"

  override def fix[A](expr: CtxString[A => A]): CtxString[A] =
    ctx => s"fix(${expr(ctx)})"

  override def lambda[A, B](name: String, expr: CtxString[B]): CtxString[B] =
    ctx => s"$name -> ${expr(s"$$$name" :: ctx)}"

  override def app[A, B](f: CtxString[A], b: CtxString[B]): CtxString[A] =
    ctx => s"app(${f(ctx)}, ${b(ctx)})"
}
