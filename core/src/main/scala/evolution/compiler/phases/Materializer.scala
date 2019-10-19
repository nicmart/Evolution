package evolution.compiler.phases
import evolution.compiler.expression.Expr
import evolution.materialization.Evolution

trait Materializer {
  def materialize[T](expr: Expr[Evolution[T]]): Long => Iterator[T]
}
