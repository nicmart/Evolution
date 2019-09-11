package evolution.compiler.impl.jsmaterialization

import evolution.compiler.phases.materializing.Materializer
import evolution.compiler.expression.Expr
import evolution.materialization.Evolution
import scala.scalajs.js

object JsCodeMaterializer extends Materializer {
  def materialize[T](expr: Expr[Evolution[T]]): Long => Iterator[T] =
    seed => evaluate(MaterializeJsCode.materialize(expr)).asInstanceOf[js.Iterable[T]].iterator

  private def evaluate(expr: String): Any = {
    val f = new js.Function(s"return $expr;")
    f.call((), ())
  }
}
