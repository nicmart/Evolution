package evolution.primitive.algebra.binding.interpreter
import evolution.algebra.representation.RNGRepr
import evolution.data.{ Annotation, Tag }
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

object BindingAnnotator extends Binding[Annotation, String] {
  private val builder = new EvolutionExpr[RNGRepr]

  override def var0[A](name: String): Annotation[A] =
    Annotation(Set(0), builder.bind.var0(name))

  override def shift[A](expr: Annotation[A]): Annotation[A] = {
    Annotation(expr.shiftedVars, builder.bind.shift(expr.expr))
  }
  override def let[A, B](name: String, value: Annotation[A], expr: Annotation[B]): Annotation[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: Annotation[A => A]): Annotation[A] =
    Annotation(expr.vars, builder.bind.fix(expr.expr))

  override def lambda[A, B](var1: String, expr: Annotation[B]): Annotation[A => B] = {
    val tag = if (expr.minVar > 0) Tag.ConstantLambda[A, B](expr.expr) else Tag.Unknown[A => B]()
    Annotation(expr.unshiftedVars, builder.bind.lambda(var1, expr.expr), tag)
  }

  override def app[A, B](f: Annotation[A => B], a: Annotation[A]): Annotation[B] =
    Annotation(f.vars ++ a.vars, builder.bind.app(f.expr, a.expr))
}
