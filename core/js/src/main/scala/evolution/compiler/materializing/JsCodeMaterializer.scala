package evolution.compiler.materializing
import evolution.compiler.phases.materializing.Materializer
import evolution.data.Expr
import evolution.materialization.Evolution
import evolution.compiler.phases.materializing.MaterializeJsCode
import scala.scalajs.js

object JsCodeMaterializer extends Materializer {
  def materialize[T](expr: Expr[Evolution[T]]): Long => Iterator[T] =
    seed => evaluate(MaterializeJsCode.materialize(expr)).asInstanceOf[js.Iterable[T]].iterator

  private def evaluate(expr: String): Any = {
    val f = new js.Function(s"return $expr;")
    f.call((), ())
  }
}
