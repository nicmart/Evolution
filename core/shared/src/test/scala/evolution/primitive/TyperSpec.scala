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
          typer.assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))) shouldBe Constraints.empty
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          typer.assignVarsAndFindConstraints(varExpr.withType(Type.Var("X"))) shouldBe Constraints.empty
        }
      }

      "pre-defined functions" - {
        "point" in {
          forAll(genNumber, genNumber) { (x, y) =>
            val p = Expr.FuncCall(PredefinedFunction.Point, List(x, y), Type.Var("X"))
            typer.findConstraints(TypeVars.empty, p)._2 shouldBe Constraints(
              p.tpe -> Type.Point,
              x.tpe -> Type.Dbl,
              y.tpe -> Type.Dbl)
          }
        }
      }

      "complex expressions" - {
        //pending
        "mapCons" in {
          val expr = Expr.FuncCall(
            PredefinedFunction.MapCons,
            List(Expr.Var("fa"), Expr.Lambda(Expr.Var("head"), Expr.Lambda(Expr.Var("tail"), Expr.Var("tail")))))

          val (vars1, withVars) = assignVars(TypeVars.empty, expr)
          val (vars2, constraints) = findConstraints(vars1, withVars)
          println(withVars)
          println(constraints)
        }

        "x -> point($x, $x)" in {
          val lambda =
            Expr.Lambda(Expr.Var("x"), Expr.FuncCall(PredefinedFunction.Point, List(Expr.Var("x"), Expr.Var("x"))))
          val (vars1, typed) = typer.assignVars(TypeVars.empty, lambda)
          typer.findConstraints(vars1, typed)._2 shouldBe Constraints.empty
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
