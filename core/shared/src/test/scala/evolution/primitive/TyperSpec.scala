package evolution.primitive
import cats.Id

class TyperSpec extends CompilerSpecModule[Id] {
  import Typer._, ast._, Expr._
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
            val p = Expr.FuncCall(PredefinedFunction.Point, List(x, y), Type.Var("X"))
            typer.findConstraints(TypeVars.empty, p)._2 shouldBe Constraints(
              p.tpe -> Type.Point,
              x.tpe -> Type.Dbl,
              y.tpe -> Type.Dbl)
          }
        }
      }

      "complex expressions" - {
        // TODO
        pending
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

    "should unify" - {
      "point expressions" in {
        val untyped = Expr.FuncCall(PredefinedFunction.Point, Expr.Var("a") :: Expr.Var("b") :: Nil)
        val (expr, constraints) = assignVarsAndFindConstraints(untyped)
        val substitution = unify(constraints).right.get
        substitution.substitute(expr).tpe shouldBe Type.Point
      }

      "app(x -> $x, 2)" in {
        val identity = Expr.Lambda(Expr.Var("x"), Expr.Var("x"))
        val untyped = Expr.FuncCall(PredefinedFunction.App, List(identity, Expr.Number("2", Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped)
        println(constraints)
        val substitution = unify(constraints).right.get
        println(substitution)
        println(expr)
        println(substitution.substitute(expr))
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
