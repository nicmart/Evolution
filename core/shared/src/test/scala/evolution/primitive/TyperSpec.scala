package evolution.primitive
import cats.Id
import cats.implicits._

class TyperSpec extends CompilerSpecModule[Id] {
  import Typer._, ast._, AST._
  val typer = Typer

  "The typer" - {
    "should generate constraints for" - {
      "numbers" in {
        forAll(genNumber) { numberExpr =>
          // Numbers literals are overloaded, we defer a decision on this
          typer.assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X")))._2 shouldBe Constraints.empty
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          typer.assignVarsAndFindConstraints(varExpr.withType(Type.Var("X")))._2 shouldBe Constraints.empty
        }
      }

      "pre-defined functions" - {
        "point" in {
          forAll(genNumber, genNumber) { (x, y) =>
            val p = AST.FuncCall(PredefinedFunction.Point, List(x, y), Type.Var("X"))
            typer.findConstraints(p).runA(TypeVars.empty).value shouldBe Constraints(
              p.tpe -> Type.Point,
              x.tpe -> Type.Dbl,
              y.tpe -> Type.Dbl)
          }
        }

        "constant evolution of a point" in {
          val point = AST.FuncCall(PredefinedFunction.Point, List(AST.Number("1"), AST.Number("1")))
          val evolution =
            typer.assignVars(AST.FuncCall(PredefinedFunction.Constant, List(point))).run(TypeVars.empty).value._2
          val constraints = typer.findConstraints(evolution).runA(TypeVars.empty).value
          val allConstraints = constraints.merge(Constraints(evolution.tpe -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints).right.get
          unifier.substitute(evolution.tpe) shouldBe Type.Evo(Type.Point)
        }
      }

      "complex expressions" - {
        // TODO
        pending
        "mapCons" in {
          val expr = AST.FuncCall(
            PredefinedFunction.MapCons,
            List(AST.Var("fa"), AST.Lambda(AST.Var("head"), AST.Lambda(AST.Var("tail"), AST.Var("tail")))))

          assignVars(expr).flatMap(findConstraints).runA(TypeVars.empty).value
        }

        "x -> point($x, $x)" in {
          val lambda =
            AST.Lambda(AST.Var("x"), AST.FuncCall(PredefinedFunction.Point, List(AST.Var("x"), AST.Var("x"))))
          assignVars(lambda).flatMap(findConstraints).runA(TypeVars.empty).value shouldBe Constraints.empty
        }
      }
    }

    "should unify" - {
      "point expressions" in {
        val untyped = AST.FuncCall(PredefinedFunction.Point, AST.Var("a") :: AST.Var("b") :: Nil)
        val (expr, constraints) = assignVarsAndFindConstraints(untyped)
        val substitution = unify(constraints).right.get
        substitution.substitute(expr).tpe shouldBe Type.Point
      }

      "app(x -> $x, 2)" in {
        val identity = AST.Lambda(AST.Var("x"), AST.Var("x"))
        val untyped = AST.FuncCall(PredefinedFunction.App, List(identity, AST.Number("2", Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped)
        val substitution = unify(constraints).right.get
        substitution.substitute(expr).tpe shouldBe Type.Dbl
      }

      "mapCons(empty, head -> tail -> cons(1, tail))" in {
        val untyped = FuncCall(
          PredefinedFunction.MapCons,
          List(
            FuncCall(PredefinedFunction.Empty, Nil),
            Lambda(
              Var("head"),
              Lambda(
                Var("tail"),
                FuncCall(PredefinedFunction.Cons, List(Number("1", Type.Dbl), Var("tail")))
              )
            )
          )
        )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped)
        val substitution = unify(constraints).right.get
        substitution.substitute(expr).tpe shouldBe Type.Evo(Type.Dbl)
      }
    }
  }
}
