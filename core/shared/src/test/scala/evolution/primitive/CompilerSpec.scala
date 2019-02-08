package evolution.primitive
import cats.Id
import evolution.data.ExpressionModule
import cats.implicits._

class CompilerSpec extends CompilerSpecModule[Id] {
  import ast._, AST._
  import AST._

  "The compiles" - {
    "should successfully compile" - {
      "number literals" in forAll(genTypedNumber) { n =>
        unsafeCompile(n) shouldBe expressionModule.Dbl(n.n.toDouble)
      }

      "variable usages" in forAll(genTypedVar) { v =>
        unsafeCompile(v) shouldBe expressionModule.Var0[v.Out](v.name)
      }

      "variable usages in non-empty contexts" in forAll(genTypedVar) { v =>
        whenever(v.name != "x") {
          unsafeCompile(v, VarContext.empty.push(v.name).push("x")) shouldBe expressionModule.Shift(
            expressionModule.Var0[v.Out](v.name))
        }
      }

      "let bindings" in forAll(genTypedVar, genTypedNumber, genTypedNumber) { (variable, n1, n2) =>
        val variable1 = AST.Var(variable.name, n1.tpe)
        unsafeCompile(AST.Let(variable1, n1, n2)) shouldBe expressionModule.Let(
          variable1.name,
          unsafeCompile(n1),
          unsafeCompile(n2)
        )
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) { (variable, n) =>
        unsafeCompile(Lambda(variable, n)) shouldBe expressionModule.Lambda(variable.name, unsafeCompile(n))
      }

    }
  }

  private def unsafeCompile(expr: AST, ctx: VarContext = VarContext.empty): expressionModule.Expr[expr.Out] =
    Compiler.compile[Either[String, ?]](expr, ctx).right.get
}
