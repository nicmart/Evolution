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
    "should generate constraints for" - {
      "numbers" in {
        forAll(genNumber) { numberExpr =>
          // Numbers literals are overloaded, we defer a decision on this
          typer.findConstraints(numberExpr.withType(Type.Var("X"))) shouldBe Nil
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          typer.findConstraints(varExpr.withType(Type.Var("X"))) shouldBe Nil
        }
      }

      "pre-defined functions" - {
        "point" in {
          forAll(genNumber, genNumber) { (x, y) =>
            val p = Expr.FuncCall("point", List(x, y), Type.Var("X"))
            typer.findConstraints(p) shouldBe List(
              Constraint(p.tpe, Type.Point),
              Constraint(x.tpe, Type.Dbl),
              Constraint(y.tpe, Type.Dbl))
          }
        }
      }
    }
  }

  def genNumber: Gen[Expr] = withRandomTypeVar(arbitrary[Double].map(d => Expr.Number(d.toString)))
  def genIntNumber: Gen[Expr] = withRandomTypeVar(arbitrary[Int].map(d => Expr.Number(d.toString)))
  def genNotIntNumber: Gen[Expr] = withRandomTypeVar(arbitrary[Int].map(d => Expr.Number((0.1 + d).toString)))

  def withRandomTypeVar(gen: Gen[Expr]): Gen[Expr] =
    for {
      expr <- gen
      typeChar <- Gen.alphaChar
    } yield expr.withType(Type.Var(typeChar.toString.toUpperCase))

  def genVar: Gen[Expr.Var] =
    for {
      char <- Gen.alphaChar
      typeChar <- Gen.alphaChar
    } yield Expr.Var(char.toString, Type.Var(typeChar.toString.toUpperCase))
}
