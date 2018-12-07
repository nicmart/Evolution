package evolution.primitive.algebra.binding.interpreter
import evolution.data.AnnotationModule._
import evolution.primitive.algebra.binding.Binding

object BindingAnnotator extends Binding[R, String] {
  override def var0[A](name: String): R[A] =
    Annotation(Set(0), builder.bind.var0(name))

  override def shift[A](expr: R[A]): R[A] = {
    Annotation(expr.shiftedVars, builder.bind.shift(expr.expr))
  }
  override def let[A, B](name: String, value: R[A], expr: R[B]): R[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: R[A => A]): R[A] =
    Annotation(expr.vars, builder.bind.fix(expr.expr))

  override def lambda[A, B](var1: String, expr: R[B]): R[A => B] =
    Annotation(expr.unshiftedVars, builder.bind.lambda(var1, expr.expr))

  override def app[A, B](f: R[A => B], a: R[A]): R[B] =
    Annotation(f.vars ++ a.vars, builder.bind.app(f.expr, a.expr))
}
