package evolution.data
import EvaluationModule._
import evolution.algebra.representation.RNGRepr
import evolution.data.Annotation.Meta
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr
import evolution.primitive.algebra.evolution.interpreter.EvolutionExpr

trait AnnotationModule {
  type F[T]
  final type R[T] = Annotation[T]
  val builder: EvolutionExpr[F] = new EvolutionExpr[F]
  def interpreter: Evolution[F, R]
  def materialize[T](seed: Long, annotation: R[F[T]]): Iterator[T]

  case class Annotation[T](vars: Set[Int], expr: Expr[F, T]) {
    def unshiftedVars: Set[Int] = vars.filter(_ > 0).map(_ - 1)
    def shiftedVars: Set[Int] = vars.map(_ + 1)
  }
}

private[data] object AnnotationModuleImpl extends AnnotationModule {
  override type F[T] = RNGRepr[T]
  override def interpreter: Evolution[F, Annotation] = ???
  override def materialize[T](seed: Long, annotation: Annotation[F[T]]): Iterator[T] = ???
}

case class Annotation[T](vars: List[Int], expr: Evaluation[T], meta: Meta[T]) {
  def isClosed: Boolean = vars.isEmpty
  def requireVar(n: Int): Boolean = vars.contains(n)
  def level: Int = vars.max
}

object Annotation {
  sealed trait Meta[T]
  case class Constant[T](value: R[T]) extends Meta[T]
  sealed trait Evo[T] extends Meta[F[T]]
  case class GenericEvo[T](evo: RNGRepr[T]) extends Evo[T]
  case class StatelessEvo[T](evo: RNGRepr[T]) extends Evo[T]
}
