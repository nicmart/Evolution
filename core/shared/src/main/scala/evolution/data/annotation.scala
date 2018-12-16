package evolution.data
import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation.Info
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr
import evolution.primitive.algebra.evolution.interpreter.EvolutionUnshifter.unshiftExpr

case class Annotation[T](vars: Set[Int], info: Annotation.Info[T]) {
  def unshiftedVars: Set[Int] = vars.filter(_ > 0).map(_ - 1)
  def shiftedVars: Set[Int] = vars.map(_ + 1)
  def minVar: Int = vars.min
  def maxVar: Int = vars.max
  def isClosed: Boolean = vars.isEmpty
  def isOpen: Boolean = !isClosed
  def evaluate(evaluator: Evolution[RNGRepr, Evaluation], ctx: Ctx): T = Annotation.evaluate(this, evaluator, ctx)
  def expr: Expr[RNGRepr, T] = info.expr
}

object Annotation {
  sealed trait Info[T] {
    def expr: Expr[RNGRepr, T]
  }

  object Info {
    final case class Unknown[T](expr: Expr[RNGRepr, T]) extends Info[T]
    final case class Fix[T](inner: Annotation[T => T], expr: Expr[RNGRepr, T]) extends Info[T]
    final case class ConstantLambda[A, B](term: Expr[RNGRepr, B], expr: Expr[RNGRepr, A => B]) extends Info[A => B]
    final case class StatelessEvolution[A](expr: Expr[RNGRepr, RNGRepr[A]]) extends Info[RNGRepr[A]]
  }

  def evaluate[T](annotation: Annotation[T], evaluator: Evolution[RNGRepr, Evaluation], ctx: Ctx): T =
    annotation.info match {
      case Info.Fix(Annotation(_, Info.ConstantLambda(term, _)), _) =>
        unshiftExpr(term).run(evaluator).evaluateWith(ctx)
      case Info.Unknown(expr)              => expr.run(evaluator).evaluateWith(ctx)
      case Info.ConstantLambda(term, expr) => expr.run(evaluator).evaluateWith(ctx)
      case Info.StatelessEvolution(expr)   => expr.run(evaluator).evaluateWith(ctx)
    }
}
