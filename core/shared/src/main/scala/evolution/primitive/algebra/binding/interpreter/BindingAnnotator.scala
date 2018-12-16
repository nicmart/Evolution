package evolution.primitive.algebra.binding.interpreter
import evolution.algebra.representation.RNGRepr
import evolution.data.{ Annotation }
import Annotation.Info._
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

object BindingAnnotator extends Binding[Annotation, String] {
  private val builder = new EvolutionExpr[RNGRepr]

  override def var0[A](name: String): Annotation[A] =
    Annotation(Set(0), Unknown(builder.bind.var0(name)))

  override def shift[A](expr: Annotation[A]): Annotation[A] = {
    Annotation(expr.shiftedVars, Unknown(builder.bind.shift(expr.expr)))
  }
  override def let[A, B](name: String, value: Annotation[A], expr: Annotation[B]): Annotation[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: Annotation[A => A]): Annotation[A] =
    Annotation(expr.vars, Unknown(builder.bind.fix(expr.expr)))

  override def lambda[A, B](var1: String, expr: Annotation[B]): Annotation[A => B] = {
    val lambdaExpr = builder.bind.lambda[A, B](var1, expr.expr)
    val info = if (expr.minVar > 0) ConstantLambda[A, B](expr.expr, lambdaExpr) else Unknown[A => B](lambdaExpr)
    Annotation(expr.unshiftedVars, info)
  }

  override def app[A, B](f: Annotation[A => B], a: Annotation[A]): Annotation[B] =
    Annotation(f.vars ++ a.vars, Unknown(builder.bind.app(f.expr, a.expr)))
}
