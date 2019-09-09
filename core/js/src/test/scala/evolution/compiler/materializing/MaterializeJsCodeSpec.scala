package evolution.compiler.materializing

import evolution.compiler.LanguageSpec
import _root_.evolution.compiler.phases.materializing.MaterializeJsCode
import _root_.evolution.data.Expr
import scala.scalajs.js.Function
import scala.scalajs.js
import evolution.compiler.phases.materializing.MaterializeJsCode.JsExpr

class MaterializeJsCodeSpec extends LanguageSpec {
  "Materializing stuff" - {
    "minimal materialization" in {
      val jsExpr = MaterializeJsCode.materialize(Expr.Dbl(1.23))
      val result = evaluate(jsExpr)
      result shouldBe 1.23
    }

    "materialize points" in {
      val jsExpr = MaterializeJsCode.materialize(Expr.Pnt(Expr.Dbl(1), Expr.Dbl(2)))
      val result = evaluate(jsExpr).asInstanceOf[js.Dictionary[Double]]
      result("x") shouldBe 1.0
      result("y") shouldBe 2.0
    }
  }

  private def evaluate(expr: JsExpr): Any = {
    val f = new Function(s" return ${expr.js};")
    f.call((), ())
  }

}
