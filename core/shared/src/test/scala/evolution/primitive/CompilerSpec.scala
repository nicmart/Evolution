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
    }
  }

  private def unsafeCompile(expr: Expr): initial.R[expr.Out] =
    Compiler.compile[initial.R](expr, alg).right.get
}
