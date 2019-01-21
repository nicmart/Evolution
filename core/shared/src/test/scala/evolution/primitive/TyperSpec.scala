package evolution.primitive
import cats.Id
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class TyperSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  val typer = new Typer[Id](new Ast[Id])
  import typer._
  import typer.ast._

  "The typer" - {
    "should find constraints for" - {
      "numbers" in {
        forAll(genNumber) { numberExpr =>
          typer.findConstraints(numberExpr.withType(Type.Var("X"))) shouldBe List(Constraint(Type.Var("X"), Type.Dbl))
        }
      }

      "integers" in {
        forAll(genIntNumber) { numberExpr =>
          val ctx = Context.empty
          val annotated = typer.check(Context.empty, Type.Integer, numberExpr).right.get
          annotated shouldBe numberExpr.withType(Type.Integer)
        }
      }

      "variables" - {}
    }
  }

  def genNumber: Gen[Expr.Number] = arbitrary[Double].map(d => Expr.Number(d.toString))
  def genIntNumber: Gen[Expr.Number] = arbitrary[Int].map(d => Expr.Number(d.toString))
  def genNotIntNumber: Gen[Expr.Number] = arbitrary[Int].map(d => Expr.Number((0.1 + d).toString))
}
