package evolution.language
import cats.Id
import cats.implicits._

class TyperModuleSpec extends LanguageSpec[Id] {
  import Typer._, Typer.TypeInference._, TypeClasses._

  "The typer" - {
    "should generate constraints for" - {
      "numbers" in {
        forAll(genNumber) { numberExpr =>
          assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))).evaluate._2 shouldBe Constraints.empty
            .withPredicate(Predicate("Num", List(Type.Var("X"))))
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          assignVarsAndFindConstraints(varExpr.withType(Type.Var("X"))).evaluate._2 shouldBe Constraints.empty
        }
      }

      "pre-defined constants" - {
        "point function" in {
          val point = assignVars(AST.Const(Constant.Point)).evaluate
          val constraints = findConstraints(point).evaluate
          val unifier = unify(constraints)
          unifier.map(_.substitution.substitute(point.tpe)) shouldBe Right(Type.Dbl =>: Type.Dbl =>: Type.Point)
        }

        "constant evolution of a point" in {
          val point = AST.App2(AST.Const(Constant.Point), AST.Number("1"), AST.Number("1"))
          val evolution =
            assignVars(AST.App(AST.Const(Constant.Constant), point)).evaluate
          val constraints = findConstraints(evolution).evaluate
          val allConstraints = constraints.merge(Constraints(evolution.tpe -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints)
          unifier.map(_.substitution.substitute(evolution.tpe)) shouldBe Right(Type.Evo(Type.Point))
        }
      }
    }

    "should unify" - {
      "point expressions" in {
        val untyped = AST.App2(AST.Const(Constant.Point), AST.Var("a"), AST.Var("b"))
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped).evaluateWith(
            Map("a" -> Qualified(Type.Dbl), "b" -> Qualified(Type.Dbl)))
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Point
      }

      "app(x -> $x, 2)" in {
        val identity = AST.Lambda("x", AST.Var("x"))
        val untyped = AST.App(identity, AST.Number("2", Type.Dbl))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).evaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Dbl
      }

      "mapCons(empty, head -> tail -> cons(1, tail))" in {
        val untyped = AST.App2(
          AST.Const(Constant.MapCons),
          AST.Const(Constant.Empty),
          AST.Lambda(
            "head",
            AST.Lambda(
              "tail",
              AST.App2(AST.Const(Constant.Cons), AST.Number("1", Type.Dbl), AST.Var("tail"))
            )
          )
        )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).evaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe shouldBe Type.Evo(Type.Dbl)
      }

      "<point>(<1>, <2>)" in {
        val untyped =
          AST.App2(AST.Lift(AST.Const(Constant.Point)), AST.Lift(AST.Number("1")), AST.Lift(AST.Number("2")))
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

          val unification = unify(constraints)
          checkPredicates(unification.right.get.substitutedPredicates).isRight shouldBe true
        }
      }
    }

    "should not unify" - {
      "predicates" - {
        "when the predicate is not valid" in {
          val constraints = Constraints(
            List(
              Constraint.Eq(Type.Var("X"), Type.Arrow(Type.Integer, Type.Integer)),
              Constraint.Pred(Predicate("Num", List(Type.Var("X"))))
            ))
          val unification = unify(constraints)
          checkPredicates(unification.right.get.substitutedPredicates).isLeft shouldBe true
        }
      }
    }
  }
}
