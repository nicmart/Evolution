package evolution.data
import EvaluationModule._
import evolution.algebra.representation.RNGRepr
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

trait AnnotationModule {
  final type F[T] = RNGRepr[T]
  final type R[T] = Annotation[T]
  val builder: EvolutionExpr[F] = new EvolutionExpr[F]
  def interpreter: Evolution[F, R]
  def materialize[T](seed: Long, annotation: R[F[T]]): Iterator[T]

  case class Annotation[T](vars: Set[Int], expr: Expr[F, T], tags: Set[T] = Set.empty[T]) {
    def unshiftedVars: Set[Int] = vars.filter(_ > 0).map(_ - 1)
    def shiftedVars: Set[Int] = vars.map(_ + 1)
  }

  sealed trait Tag[T]
  case class Constant[A](term: Expr[F, A]) extends Tag[A]
  case class ConstantLambda[A, B](term: Expr[F, B]) extends Tag[A => B]
}

private[data] object AnnotationModuleImpl extends AnnotationModule {
  override def interpreter: Evolution[F, Annotation] = ???
  override def materialize[T](seed: Long, annotation: Annotation[F[T]]): Iterator[T] = ???
}
