package evolution.language
import cats.Id
import cats.implicits._

class CompilerModuleSpec extends LanguageSpec[Id] {
  import Expr._

  "The compiler" - {
    "should successfully compile" - {
      "number literals" in forAll(genTypedNumber) { n =>
        unsafeCompile(n) shouldBe Dbl(n.n.toDouble)
      }

      "variable usages" in forAll(genTypedVar) { v =>
        unsafeCompile(v, VarContext.empty.push(v.name)) shouldBe Var[v.Out](v.name)
      }

      "variable usages in non-empty contexts" in forAll(genTypedVar) { v =>
        whenever(v.name != "x") {
          unsafeCompile(v, VarContext.empty.push(v.name).push("x")) shouldBe Var[v.Out](v.name)
        }
      }

      "let bindings" in forAll(genTypedVar, genTypedNumber, genTypedNumber) { (variable, n1, n2) =>
        val variable1 = AST.Var(variable.name, n1.tpe)
        unsafeCompile(AST.Let(variable1, n1, n2)) shouldBe Let(
          variable1.name,
          unsafeCompile(n1),
          unsafeCompile(n2)
        )
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) { (variable, n) =>
        unsafeCompile(AST.Lambda(variable, n)) shouldBe Lambda(variable.name, unsafeCompile(n))
      }

    }
  }

  private def unsafeCompile(expr: AST, ctx: VarContext = VarContext.empty): Expr[expr.Out] =
    Compiler.compile[Either[String, ?]](expr).run(ctx).right.get
}
