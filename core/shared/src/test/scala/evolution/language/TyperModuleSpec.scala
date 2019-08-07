package evolution.language
import cats.implicits._
import cats.mtl.implicits._
import cats.Monad
import scala.util.Random
import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import Typer._
import Instances._
import evolution.compiler.ast.AST
import evolution.compiler.phases.typing.Constraints
import evolution.compiler.phases.typing.Constraint
import evolution.compiler.phases.typing.Substitution

class TyperModuleSpec extends LanguageSpec {

  // TODO this is to avoid ambiguities. Can we do better than that?
  implicit val applicative: Monad[TypeInferenceResult] = typeInference.S.monad

  "The typer" - {
    "should generate constraints for" - {
      "int literals" in {
        forAll(genIntNumber) { numberExpr =>
          assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))).unsafeEvaluate._2 shouldBe Constraints.empty
            .withPredicate(Predicate("Num", List(Type.Var("X"))))
        }
      }

      "double literals" in {
        forAll(genDoubleNotIntNumber) { numberExpr =>
          assignVarsAndFindConstraints(numberExpr.withType(Type.Var("X"))).unsafeEvaluate._2 shouldBe Constraints(
            Type.Var("X") -> Type.Dbl
          )
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
          val unifier = unify[TypeInferenceResult](constraints)
          unifier.map(_.substitution.substitute(point.tpe)).evaluateEither() shouldBe Right(
            Qualified(Type.Dbl =>: Type.Dbl =>: Type.Point)
          )
        }

        "constant evolution of a point" in {
          val point = AST.AppN(AST.Const(Constant2.Point), AST.DoubleLiteral(1), AST.DoubleLiteral(1))
          val evolution =
            assignVars(AST.App(AST.Const(Constant1.Constant), point)).unsafeEvaluate
          val constraints = findConstraints(evolution).unsafeEvaluate
          val allConstraints = constraints.merge(Constraints(evolution.tpe.t -> Type.Evo(Type.Point)))
          val unifier = unify[TypeInferenceResult](allConstraints)
          unifier.map(_.substitution.substitute(evolution.tpe)).evaluateEither() shouldBe Right(
            Qualified(Type.Evo(Type.Point))
          )
        }
      }
    }

    "should unify" - {
      "point expressions" in {
        val untyped = AST.AppN(AST.Const(Constant2.Point), AST.Identifier("a"), AST.Identifier("b"))
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped).unsafeEvaluateWith(
            new TypeBindings(
              Map(
                "a" -> TypeBinding.Variable("a", Qualified(Type.Dbl)),
                "b" -> TypeBinding.Variable("b", Qualified(Type.Dbl))
              )
            )
          )
        val substitution = unify[TypeInferenceResult](constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Point
      }

      "app(x -> x, 2)" in {
        val identity = AST.Lambda("x", AST.Identifier("x"))
        val untyped = AST.App(identity, AST.DoubleLiteral(2, Qualified(Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify[TypeInferenceResult](constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Dbl
      }

      "mapCons(empty, head -> tail -> cons(1, tail))" in {
        val untyped = AST.AppN(
          AST.Const(Constant2.MapCons),
          AST.Const(Constant0.Empty),
          AST.Lambda(
            "head",
            AST.Lambda(
              "tail",
              AST.AppN(AST.Const(Constant2.Cons), AST.DoubleLiteral(1, Qualified(Type.Dbl)), AST.Identifier("tail"))
            )
          )
        )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify[TypeInferenceResult](constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Evo(Type.Dbl)
      }

      "@(1)" in {
        val untyped = AST.App(AST.Const(Constant1.Constant), AST.DoubleLiteral(1, Qualified(Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify[TypeInferenceResult](constraints).unsafeEvaluate.substitution
        val finalExpr = substitution.substitute(expr)
        finalExpr.tpe.t shouldBe Type.Evo(Type.Dbl)

        val AST.App(AST.Identifier(_, _, isPrimitive), _, _) = finalExpr
        isPrimitive shouldBe true
      }

      "@point(@(1), @(2))" in {
        val untyped =
          AST.AppN(
            AST.Const(Constant2.LiftedPoint),
            AST.App(AST.Const(Constant1.Constant), AST.DoubleLiteral(1)),
            AST.App(AST.Const(Constant1.Constant), AST.DoubleLiteral(2))
          )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify[TypeInferenceResult](constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Evo(Type.Point)
      }

      "@((x -> 2 * x)(1))" in {
        val untyped =
          AST.App(
            AST.Const(Constant1.Constant),
            AST.App(
              AST.Lambda(
                "x",
                AST.AppN(
                  AST.Const(Constant2.Multiply),
                  AST.DoubleLiteral(2),
                  AST.Identifier("x", Qualified(Type.Point))
                )
              ),
              AST.AppN(AST.Const(Constant2.Point), AST.DoubleLiteral(1), AST.DoubleLiteral(2))
            )
          )

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val unification = unify[TypeInferenceResult](constraints).unsafeEvaluate
        val subst = predicatesSubstitution(unification.predicates).unsafeEvaluate.compose(unification.substitution)
        val typedExpr = subst.substitute(expr)
        typedExpr.tpe.t shouldBe Type.Evo(Type.Point)
      }

      "solve1(@(x -> x), point(0, 0))" in {
        val untyped =
          AST.AppN(
            AST.Const(Constant2.Solve1),
            AST.App(AST.Const(Constant1.Constant), AST.Lambda("x", AST.Identifier("x"))),
            AST.AppN(AST.Const(Constant2.Point), AST.DoubleLiteral(1), AST.DoubleLiteral(2))
          )

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify[TypeInferenceResult](constraints).unsafeEvaluate.substitution
        val typedExpr = substitution.substitute(expr)
        typedExpr.tpe.t shouldBe Type.Evo(Type.Point)
      }

      "predicates" - {
        "when the predicate is valid" in {
          val constraints = Constraints(
            List(
              Constraint.Eq(Type.Var("X"), Type.Dbl),
              Constraint.Pred(Predicate("Num", List(Type.Var("X"))))
            )
          )

          val unification = unify[TypeInferenceResult](constraints)
          predicatesSubstitution(unification.unsafeEvaluate.substitutedPredicates)
            .evaluateEither()
            .isRight shouldBe true
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
            )
          )
          val unification = unify[TypeInferenceResult](constraints)
          predicatesSubstitution(unification.unsafeEvaluate.substitutedPredicates).evaluateEither().isLeft shouldBe true
        }
      }
    }
  }

  "predicates unification" - {
    "should succeed with an empty substitution if there are no predicates" in {
      val subst = PredicatesUnifier.unify(instances, Nil)

      subst shouldBe Some(Substitution.empty)
    }

    "should succeed with an empty substitution if there is a single predicate that is the same as an instance" in {
      val subst = PredicatesUnifier.unify(instances, instances.take(1))

      subst shouldBe Some(Substitution.empty)
    }

    "should succeed with a simple substitution if there is a single predicate with vars and a matching instance" in {
      val predicates = List(Predicate("Num", List(Type.Var("X"))))
      val instances = List(Predicate("Num", List(Type.Dbl)))
      val subst = PredicatesUnifier.unify(instances, predicates)

      subst shouldBe Some(Substitution("X" -> Type.Dbl))
    }

    "should succeed with higher-horder types in predicates" in {
      val predicates = List(Predicate("Num", List(Type.Evo(Type.Var("X")))))
      val instances = List(Predicate("Num", List(Type.Evo(Type.Dbl))))
      val subst = PredicatesUnifier.unify(instances, predicates)

      subst shouldBe Some(Substitution("X" -> Type.Dbl))
    }

    "should do more complex unifications" in {
      val predicates = List(
        Predicate("Num", List(Type.Var("Y"))),
        Predicate("Both", List(Type.Var("X"), Type.Var("Y")))
      )

      val instances = List(
        Predicate("Num", List(Type.Point)),
        Predicate("Num", List(Type.Integer)),
        Predicate("Both", List(Type.Dbl, Type.Dbl)),
        Predicate("Both", List(Type.Integer, Type.Integer))
      )
      val subst = PredicatesUnifier.unify(instances, predicates).get

      subst.substitute[Type](Type.Var("X")) shouldBe Type.Integer
      subst.substitute[Type](Type.Var("Y")) shouldBe Type.Integer
    }

    // This is mostly to test perfomance of predicates unification
    "should uniffy predicates of @(point(0, 1 * 2 * 3 * 4 * 5 * 6 * 7 * 8))" in {
      val predicates = List(
        Predicate("Num", List(Type.Dbl)),
        Predicate("Mult", List(Type.Var("T1"), Type.Var("T7"), Type.Dbl)),
        Predicate("Num", List(Type.Var("T1"))),
        Predicate("Mult", List(Type.Var("T2"), Type.Var("T6"), Type.Var("T7"))),
        Predicate("Num", List(Type.Var("T2"))),
        Predicate("Mult", List(Type.Var("T8"), Type.Var("T5"), Type.Var("T6"))),
        Predicate("Num", List(Type.Var("T8"))),
        Predicate("Mult", List(Type.Var("T3"), Type.Var("T4"), Type.Var("T5"))),
        Predicate("Num", List(Type.Var("T3"))),
        Predicate("Mult", List(Type.Var("T9"), Type.Var("T10"), Type.Var("T4"))),
        Predicate("Num", List(Type.Var("T9"))),
        Predicate("Mult", List(Type.Var("T11"), Type.Var("T12"), Type.Var("T0"))),
        Predicate("Num", List(Type.Var("T11"))),
        Predicate("Mult", List(Type.Var("T13"), Type.Var("T14"), Type.Var("T12"))),
        Predicate("Num", List(Type.Var("T13"))),
        Predicate("Num", List(Type.Var("T14")))
      )

      val subst = PredicatesUnifier.unify(instances, Random.shuffle(predicates)).get

      subst.substitute[Type](Type.Var("T4")) shouldBe Type.Dbl
    }

    "should fail if there are no instances and there is at least one predicate" in {
      val predicates = List(Predicate("Num", List(Type.Var("X"))))
      val result = PredicatesUnifier.unify(Nil, predicates)

      result shouldBe None
    }

    "should fail if the only instance does not match the typeclass of the only predicate" in {
      val instances = List(Predicate("Num", List(Type.Integer)))
      val predicates = List(Predicate("Whatever", List(Type.Integer)))
      val result = PredicatesUnifier.unify(instances, predicates)

      result shouldBe None
    }

    "should fail if the only instance does not match the types of the only predicate" in {
      val instances = List(Predicate("Num", List(Type.Integer)))
      val predicates = List(Predicate("Num", List(Type.Dbl)))
      val result = PredicatesUnifier.unify(instances, predicates)

      result shouldBe None
    }

    "should fail if the the matching instance leads to incompativle assignments" in {
      val instances = List(Predicate("Bi", List(Type.Integer, Type.Dbl)))
      val predicates = List(Predicate("Bi", List(Type.Var("x"), Type.Var("x"))))
      val result = PredicatesUnifier.unify(instances, predicates)

      result shouldBe None
    }
  }
}
