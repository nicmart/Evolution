package evolution.language
import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.ast.AST
import evolution.compiler.phases.typing.FindConstraints
import evolution.compiler.phases.typing.AssignFreshTypeVars
import evolution.compiler.phases.typing.UnifyTypes.unify
import evolution.compiler.phases.typing.config.{ Constant0, Constant1, Constant2, TypingConfig }
import evolution.compiler.phases.typing.model.Constraints

class UnifyTypesSpec extends LanguageSpec {

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
          val point =
            AssignFreshTypeVars.assign(AST.Const(Constant2.Point), TypingConfig.constantQualifiedTypes)
          val constraints = FindConstraints.find(point).unsafeEvaluate
          val unifier = unify(constraints)
          unifier.map(_.substitution.substitute(point.tpe)) shouldBe Right(
            Qualified(Type.Dbl =>: Type.Dbl =>: Type.Point)
          )
        }

        "constant evolution of a point" in {
          val point = AST.AppN(AST.Const(Constant2.Point), AST.DoubleLiteral(1), AST.DoubleLiteral(1))
          val evolution =
            AssignFreshTypeVars.assign(
              AST.App(AST.Const(Constant1.Constant), point),
              TypingConfig.constantQualifiedTypes
            )
          val constraints = FindConstraints.find(evolution).unsafeEvaluate
          val allConstraints = constraints.merge(Constraints(evolution.tpe.t -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints)
          unifier.map(_.substitution.substitute(evolution.tpe)) shouldBe Right(
            Qualified(Type.Evo(Type.Point))
          )
        }
      }
    }

    "should unify" - {
      "point expressions" in {
        val untyped = AST.AppN(AST.Const(Constant2.Point), AST.Identifier("a"), AST.Identifier("b"))
        val extraBindings = new TypeBindings(
          Map(
            "a" -> TypeBinding.Variable("a", Qualified(Type.Dbl)),
            "b" -> TypeBinding.Variable("b", Qualified(Type.Dbl))
          )
        )
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped, extraBindings).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Point
      }

      "app(x -> x, 2)" in {
        val identity = AST.Lambda("x", AST.Identifier("x"))
        val untyped = AST.App(identity, AST.DoubleLiteral(2, Qualified(Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
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
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Evo(Type.Dbl)
      }

      "@(1)" in {
        val untyped = AST.App(AST.Const(Constant1.Constant), AST.DoubleLiteral(1, Qualified(Type.Dbl)))
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val finalExpr = substitution.substitute(expr)
        finalExpr.tpe.t shouldBe Type.Evo(Type.Dbl)

        val AST.App(AST.Identifier(_, _, isPrimitive), _, _) = finalExpr
        isPrimitive shouldBe true
      }

      "@point(const(1), const(2))" in {
        val untyped =
          AST.AppN(
            AST.Const(Constant2.LiftedPoint),
            AST.App(AST.Const(Constant1.Constant), AST.DoubleLiteral(1)),
            AST.App(AST.Const(Constant1.Constant), AST.DoubleLiteral(2))
          )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).tpe.t shouldBe Type.Evo(Type.Point)
      }

      "solve1(const(x -> x), point(0, 0))" in {
        val untyped =
          AST.AppN(
            AST.Const(Constant2.Solve1),
            AST.App(AST.Const(Constant1.Constant), AST.Lambda("x", AST.Identifier("x"))),
            AST.AppN(AST.Const(Constant2.Point), AST.DoubleLiteral(1), AST.DoubleLiteral(2))
          )

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val typedExpr = substitution.substitute(expr)
        typedExpr.tpe.t shouldBe Type.Evo(Type.Point)
      }
    }
  }

  def assignVarsAndFindConstraints(
    expr: AST,
    extraTypeBindings: TypeBindings = TypeBindings.empty
  ): Either[String, (AST, Constraints)] = {

    val exprWithVars = AssignFreshTypeVars.assign(expr, TypingConfig.constantQualifiedTypes.merge(extraTypeBindings))

    for {
      constraints <- FindConstraints.find(exprWithVars)
    } yield (exprWithVars, constraints)
  }
}
