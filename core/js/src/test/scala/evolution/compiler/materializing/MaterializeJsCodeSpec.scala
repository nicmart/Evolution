package evolution.compiler.materializing

import evolution.compiler.LanguageSpec
import _root_.evolution.compiler.phases.materializing.MaterializeJsCode
import _root_.evolution.data.Expr

class MaterializeJsCodeSpec extends LanguageSpec {
  "Materializing stuff" - {
    "minimal materialization" in {
      val jsExpr = MaterializeJsCode.materialize(Expr.Dbl(1.23))

      jsExpr.js shouldBe "1.23"
    }
  }
}
