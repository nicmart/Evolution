package evolution.primitive
import evolution.data.initial
import evolution.primitive.algebra.evolution.Evolution

class CompilerSpec extends CompilerSpecModule[initial.F] {
  val alg: Evolution[initial.F, initial.R] = initial.evolution
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

      "let bindings" in forAll(genTypedVar, genTypedNumber, genTypedNumber) { (variable, n1, n2) =>
        val variable1 = Expr.Var(variable.name, n1.tpe)
        unsafeCompile(Expr.Let(variable1, n1, n2)) shouldBe initial.Let(
          variable1.name,
          unsafeCompile(n1),
          unsafeCompile(n2))
      }

      "lambdas" in forAll(genTypedVar, genTypedNumber) { (variable, n) =>
        unsafeCompile(Lambda(variable, n)) shouldBe initial.Lambda(variable.name, unsafeCompile(n))
      }
    }
  }

  private def unsafeCompile(expr: Expr): initial.R[expr.Out] =
    Compiler.compile[initial.R](expr, alg).right.get
}
