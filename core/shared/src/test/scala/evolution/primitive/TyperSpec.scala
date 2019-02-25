package evolution.primitive
import cats.Id
import cats.implicits._

class TyperSpec extends CompilerSpecModule[Id] {
  import Typer._, ast._, AST._, Typer.TypeInference._, TypeClasses._

  "The typer" - {
    "should generate constraints for" - {
      "numbers" in {
        forAll(genNumber) { numberExpr =>
          // Numbers literals are overloaded, we defer a decision on this
          assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))).evaluate._2 shouldBe Constraints.empty
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          assignVarsAndFindConstraints(varExpr.withType(Type.Var("X"))).evaluate._2 shouldBe Constraints.empty
        }
      }

      "pre-defined constants" - {

        "constant evolution of a point" in {
          val point = AST.App2(AST.Const(PredefinedConstant.Point), AST.Number("1"), AST.Number("1"))
          val evolution =
            assignVars(AST.App(AST.Const(PredefinedConstant.Constant), point)).evaluate
          val constraints = findConstraints(evolution).evaluate
          val allConstraints = constraints.merge(Constraints(evolution.tpe -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints).right.get.substitution
          unifier.substitute(evolution.tpe) shouldBe Type.Evo(Type.Point)
        }
      }
    }

    "should unify" - {
      "point expressions" in {
        val untyped = AST.App2(AST.Const(PredefinedConstant.Point), AST.Var("a"), AST.Var("b"))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).evaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Point
      }

      "app(x -> $x, 2)" in {
        val identity = AST.Lambda(AST.Var("x"), AST.Var("x"))
        val untyped = AST.App(identity, AST.Number("2", Type.Dbl))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).evaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Dbl
      }

      "mapCons(empty, head -> tail -> cons(1, tail))" in {
        val untyped = App2(
          AST.Const(PredefinedConstant.MapCons),
          AST.Const(PredefinedConstant.Empty),
          Lambda(
            Var("head"),
            Lambda(
              Var("tail"),
              App2(AST.Const(PredefinedConstant.Cons), Number("1", Type.Dbl), Var("tail"))
            )
          )
        )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).evaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Evo(Type.Dbl)
      }

      "<point>(<1>, <2>)" in {
        val untyped = App2(Lift(Const(PredefinedConstant.Point)), Lift(Number("1")), Lift(Number("2")))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).evaluate
        println(unify(constraints))
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Evo(Type.Point)
      }

      "predicates" - {
        "when the predicate is valid" in {
          val constraints = Constraints(
            List(
              Constraint.Eq(Type.Var("X"), Type.Dbl),
              Constraint.Pred(Predicate("Num", List(Type.Var("X"))))
            ))
          val substitution = unify(constraints)
          substitution.isRight shouldBe true
          substitution.right.get.substitution.substitute[Type](Type.Var("X")) shouldBe Type.Dbl
        }
      }
    }

    "should not unify" - {
      "predicates" - {
        "when the predicate is not valid" in {}
      }
    }
  }
}
