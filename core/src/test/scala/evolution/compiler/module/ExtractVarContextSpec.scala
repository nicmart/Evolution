package evolution.compiler.module
import evolution.compiler.LanguageSpec
import evolution.compiler.expression.Expr
import evolution.compiler.phases.checkvars.model.VarContext

class ExtractVarContextSpec extends LanguageSpec {
  "ExtractVarContext" - {
    "extracts an empty context if there are no bindings" in {
      ExtractVarContext(export) shouldEq VarContext.empty
    }

    "extracts the immediate wrapping let" in {
      val expr = Expr.Let("myvar", Expr.Dbl(1), export)
      ExtractVarContext(expr) shouldEq VarContext.empty.push("myvar")
    }

    "extracts all the wrapping lets" in {
      val expr = Expr.Let("myvar2", Expr.Dbl(1), Expr.Let("myvar1", Expr.Dbl(1), export))
      ExtractVarContext(expr) shouldEq VarContext.empty.push("myvar2").push("myvar1")
    }

    "ignore bindings inside definitions" in {
      val expr = Expr.Let("myvar1", Expr.Let("myvar2", Expr.Dbl(1), export), export)
      ExtractVarContext(expr) shouldEq VarContext.empty.push("myvar1")
    }
  }

  lazy val export = Expr.Var("export")
}
