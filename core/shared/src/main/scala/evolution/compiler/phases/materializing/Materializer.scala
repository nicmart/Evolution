package evolution.compiler.phases.materializing
import evolution.compiler.expression.Expr
import evolution.materialization.Evolution

trait Materializer {
  def materialize[T](expr: Expr[Evolution[T]]): Long => Iterator[T]
}
