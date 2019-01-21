package evolution.primitive
import cats.Id
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class TyperSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  val typer = new Typer[Id](new Ast[Id])
  import typer.ast._

  "The typer" - {
    "should successfully type" - {
      "all numbers as doubles" in {
        forAll(genNumber) { numberExpr =>
          val ctx = Context.empty
          val annotated = typer.check(Context.empty, Type.Dbl, numberExpr).right.get
          annotated shouldBe numberExpr.copy(tpe = Typed(Type.Dbl))
        }
      }

      "integers as int" in {
        forAll(genIntNumber) { numberExpr =>
          val ctx = Context.empty
          val annotated = typer.check(Context.empty, Type.Integer, numberExpr).right.get
          annotated shouldBe numberExpr.copy(tpe = Typed(Type.Integer))
        }
      }

      "variables" - {}
    }

    "should fail to type" - {
      "doubles as integers" in {
        forAll(genNotIntNumber) { numberExpr =>
          val ctx = Context.empty
          val annotated = typer.check(Context.empty, Type.Integer, numberExpr)
          annotated.isLeft shouldBe true
        }
      }
    }
  }

  def genNumber: Gen[Expr.Number] = arbitrary[Double].map(d => Expr.Number(d.toString))
  def genIntNumber: Gen[Expr.Number] = arbitrary[Int].map(d => Expr.Number(d.toString))
  def genNotIntNumber: Gen[Expr.Number] = arbitrary[Int].map(d => Expr.Number((0.1 + d).toString))
}
