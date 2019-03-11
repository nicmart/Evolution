package evolution.language
import cats.Id
import cats.implicits._

class TyperModuleSpec extends LanguageSpec[Id] {
  import Typer._, Typer.TypeInference._, TypeClasses._

  "The typer" - {
    "should generate constraints for" - {
      "numbers" in {
        forAll(genNumber) { numberExpr =>
          assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X")))
        //assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))).unsafeEvaluate shouldBe 1
//          assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))).unsafeEvaluate._2 shouldBe Constraints.empty
//            .withPredicate(Predicate("Num", List(Type.Var("X"))))
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          assignVarsAndFindConstraints(varExpr.withType(Type.Var("X"))).unsafeEvaluate._2 shouldBe Constraints.empty
        }
      }

      "pre-defined constants" - {
        "point function" in {
          val point = assignVars(AST.Const(Constant2.Point)).unsafeEvaluate
          val constraints = findConstraints(point).unsafeEvaluate
          val unifier = unify(constraints)
          unifier.map(_.substitution.substitute(point.tpe)) shouldBe Right(
            Qualified(Type.Dbl =>: Type.Dbl =>: Type.Point))
        }

        "constant evolution of a point" in {
          val point = AST.App2(AST.Const(Constant2.Point), AST.Number("1"), AST.Number("1"))
          val evolution =
            assignVars(AST.App(AST.Const(Constant1.Constant), point)).unsafeEvaluate
          val constraints = findConstraints(evolution).unsafeEvaluate
          val allConstraints = constraints.merge(Constraints(evolution.tpe.t -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints)
          unifier.map(_.substitution.substitute(evolution.tpe)) shouldBe Right(Qualified(Type.Evo(Type.Point)))
        }
      }
    }

    "should unify" - {
      "point expressions" in {
        val untyped = AST.App2(AST.Const(Constant2.Point), AST.Identifier("a"), AST.Identifier("b"))
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped).unsafeEvaluateWith(
            Map("a" -> Binding.Variable("a", Qualified(Type.Dbl)), "b" -> Binding.Variable("b", Qualified(Type.Dbl))))
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Point
      }

      "app(x -> x, 2)" in {
        val identity = AST.Lambda("x", AST.Identifier("x"))
        val untyped = AST.App(identity, AST.Number("2", Qualified(Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Dbl
      }

      "mapCons(empty, head -> tail -> cons(1, tail))" in {
        val untyped = AST.App2(
          AST.Const(Constant2.MapCons),
          AST.Const(Constant0.Empty),
          AST.Lambda(
            "head",
            AST.Lambda(
              "tail",
              AST.App2(AST.Const(Constant2.Cons), AST.Number("1", Qualified(Type.Dbl)), AST.Identifier("tail"))
            )
          )
        )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Evo(Type.Dbl)
      }

      "@1" in {
        val untyped = AST.Lift(AST.Number("1", Qualified(Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).right.get.substitution
        val finalExpr = substitution.substitute(expr)
        finalExpr.tpe.t shouldBe Type.Evo(Type.Dbl)

        val AST.App(AST.Identifier(id, _, isPrimitive), x, _) = finalExpr
        isPrimitive shouldBe true
      }

      "@point(@1, @2)" in {
        val untyped =
          AST.App2(AST.Lift(AST.Const(Constant2.Point)), AST.Lift(AST.Number("1")), AST.Lift(AST.Number("2")))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).right.get.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Evo(Type.Point)
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
