package evolution.primitive
import cats.Id
import evolution.data.initial
import evolution.primitive.algebra.evolution.Evolution
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary

class CompilerSpec
    extends FreeSpec
    with Matchers
    with GeneratorDrivenPropertyChecks
    with CompilerModule[initial.F]
    with TyperModule[initial.F]
    with ParsersModule[initial.F]
    with ASTArbitraries[initial.F]
    with WithAst[initial.F] {
  val alg: Evolution[initial.F, initial.R] = initial.evolution
  import ast._, Expr._
  import Expr._

  "The compiles" - {
    "should successfully compile" - {
      "number literals" in forAll(arbitrary[Double]) { d =>
        val n = Number(d.toString, Type.Dbl)
        unsafeCompile(n) shouldBe initial.Dbl(d)
      }
    }
  }

  private def unsafeCompile(expr: Expr): initial.R[_] =
    Compiler.compile[initial.R](expr, alg).right.get
}
