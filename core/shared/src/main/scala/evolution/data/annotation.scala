package evolution.data
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.primitive.algebra.evolution.interpreter.EvolutionUnshifter.unshiftExpr

case class Annotation[T](vars: Set[Int], expr: Expr[RNGRepr, T], tag: Tag[T] = Tag.Unknown[T]()) {
  def unshiftedVars: Set[Int] = vars.filter(_ > 0).map(_ - 1)
  def shiftedVars: Set[Int] = vars.map(_ + 1)
  def minVar: Int = vars.min
  def maxVar: Int = vars.max
  def isClosed: Boolean = vars.isEmpty
  def isOpen: Boolean = !isClosed
  def evaluate(evaluator: Evolution[RNGRepr, Evaluation], ctx: Ctx): T = tag match {
    case Tag.Constant(term) => term.run[Evaluation](evaluator).evaluate
    case Tag.Fix(Annotation(_, _, Tag.ConstantLambda(term))) =>
      unshiftExpr(term).run(evaluator).evaluateWith(ctx)
    case _ => expr.run(evaluator).evaluateWith(ctx)
  }
}

sealed trait Tag[T]
object Tag {
  final case class Unknown[T]() extends Tag[T]
  final case class Fix[A](inner: Annotation[A => A]) extends Tag[A]
  final case class Constant[A](term: Expr[RNGRepr, A]) extends Tag[A]
  final case class ConstantLambda[A, B](term: Expr[RNGRepr, B]) extends Tag[A => B]
}
