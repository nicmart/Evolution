package evolution.primitive
import cats.Id
import evolution.data.Initial
import cats.implicits._

class CompilerSpec extends CompilerSpecModule[Id] {
  import ast._, Expr._
  import Expr._

  "The compiles" - {
    "should successfully compile" - {
      "number literals" in forAll(genTypedNumber) { n =>
        unsafeCompile(n) shouldBe initial.Dbl(n.n.toDouble)
      }

      "variable usages" in forAll(genTypedVar) { v =>
        unsafeCompile(v) shouldBe initial.Var0[v.Out](v.name)
      }

      "variable usages in non-empty contexts" in forAll(genTypedVar) { v =>
        whenever(v.name != "x") {
          unsafeCompile(v, VarContext.empty.push(v.name).push("x")) shouldBe initial.Shift(initial.Var0[v.Out](v.name))
        }
      }

      "let bindings" in forAll(genTypedVar, genTypedNumber, genTypedNumber) { (variable, n1, n2) =>
        val variable1 = Expr.Var(variable.name, n1.tpe)
        unsafeCompile(Expr.Let(variable1, n1, n2)) shouldBe initial.Let(
          variable1.name,
          unsafeCompile(n1),
          unsafeCompile(n2)
        )
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) { (variable, n) =>
        unsafeCompile(Lambda(variable, n)) shouldBe initial.Lambda(variable.name, unsafeCompile(n))
      }

    }
  }

  private def unsafeCompile(expr: Expr, ctx: VarContext = VarContext.empty): initial.R[expr.Out] =
    Compiler.compile[Either[String, ?]](expr, ctx).right.get
}
