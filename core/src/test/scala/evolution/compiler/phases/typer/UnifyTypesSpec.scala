package evolution.compiler.phases.typer

import evolution.compiler.types._
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.typer.FindConstraints
import evolution.compiler.phases.typer.AssignFreshTypeVars
import evolution.compiler.phases.typer.UnifyTypes.unify
import evolution.compiler.phases.typer.config.{ Constant1, Constant2, TypingConfig }
import evolution.compiler.phases.typer.model.Constraints
import evolution.compiler.tree.TreeF._
import evolution.compiler.tree._
import evolution.compiler.LanguageSpec

class UnifyTypesSpec extends LanguageSpec {

  "UnifyTypes" - {

    "should unify" - {

      "pre-defined constants" - {
        "point function" in {
          val point =
            AssignFreshTypeVars.assign(Identifier.const(Constant2.Point).embed, TypingConfig.constantQualifiedTypes)
          val constraints = FindConstraints.find(point).unsafeEvaluate
          val unifier = unify(constraints)
          unifier.map(_.substitution.substitute(point.annotation)) shouldBe Right(
            Qualified(Type.Double =>: Type.Double =>: Type.Point)
          )
        }

        "constant evolution of a point" in {
          val point =
            App.of(Identifier.const(Constant2.Point).embed, DoubleLiteral(1).embed, DoubleLiteral(1).embed).embed
          val evolution =
            AssignFreshTypeVars.assign(
              App.of(Identifier.const(Constant1.Constant).embed, point).embed,
              TypingConfig.constantQualifiedTypes
            )
          val constraints = FindConstraints.find(evolution).unsafeEvaluate
          val allConstraints = constraints.merge(Constraints(evolution.annotation.value -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints)
          unifier.map(_.substitution.substitute(evolution.annotation)) shouldBe Right(
            Qualified(Type.Evo(Type.Point))
          )
        }
      }

      "point expressions" in {
        val untyped =
          App.of(Identifier.const(Constant2.Point).embed, Identifier("a").embed, Identifier("b").embed).embed
        val extraAssumptions = new Assumptions(
          Map(
            "a" -> Assumption("a", Qualified(Scheme(Type.Double)), false),
            "b" -> Assumption("b", Qualified(Scheme(Type.Double)), false)
          )
        )
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped, extraAssumptions).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).annotation.value shouldBe Type.Point
      }

      "app(x -> x, 2)" in {
        val identity = Lambda("x", Identifier("x").embed).embed
        val untyped = App.of(identity, DoubleLiteral(2).embed).embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).annotation.value shouldBe Type.Double
      }

      "const(1)" in {
        val untyped = App.of(Identifier.const(Constant1.Constant).embed, DoubleLiteral(1).embed).embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val finalExpr = substitution.substitute(expr)
        finalExpr.annotation.value shouldBe Type.Evo(Type.Double)

        val AnnotatedTree(_, App(AnnotatedTree(_, Identifier(_, isPrimitive)), _)) = finalExpr
        isPrimitive shouldBe true
      }

      "@point(const(1), const(2))" in {
        val untyped =
          App
            .of(
              Identifier.const(Constant2.LiftedPoint).embed,
              App.of(Identifier.const(Constant1.Constant).embed, DoubleLiteral(1).embed).embed,
              App.of(Identifier.const(Constant1.Constant).embed, DoubleLiteral(2).embed).embed
            )
            .embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).annotation.value shouldBe Type.Evo(Type.Point)
      }

      "solve1(const(x -> x), point(0, 0))" in {
        val untyped =
          App
            .of(
              Identifier.const(Constant2.Solve1).embed,
              App.of(Identifier.const(Constant1.Constant).embed, Lambda("x", Identifier("x").embed).embed).embed,
              App.of(Identifier.const(Constant2.Point).embed, DoubleLiteral(1).embed, DoubleLiteral(2).embed).embed
            )
            .embed

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val typedExpr = substitution.substitute(expr)
        typedExpr.annotation.value shouldBe Type.Evo(Type.Point)
      }

      "uniformChoice(point(1, 2))" in {
        val untyped =
          App
            .of(
              Identifier.const(Constant1.UniformChoice).embed,
              Lst(
                List[Tree](
                  App.of(Identifier.const(Constant2.Point).embed, IntLiteral(1).embed, IntLiteral(2).embed).embed
                )
              ).embed
            )
            .embed

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val typedExpr = substitution.substitute(expr)
        typedExpr.annotation.value shouldBe Type.Evo(Type.Point)
      }
    }
  }

  def assignVarsAndFindConstraints(
    expr: Tree,
    extraAssumptions: Assumptions = Assumptions.empty
  ): Either[String, (TypedTree, Constraints)] = {

    val exprWithVars = AssignFreshTypeVars.assign(expr, TypingConfig.constantQualifiedTypes.merge(extraAssumptions))

    for {
      constraints <- FindConstraints.find(exprWithVars)
    } yield (exprWithVars, constraints)
  }
}
