package evolution.compiler

import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.typing.FindConstraints
import evolution.compiler.phases.typing.AssignFreshTypeVars
import evolution.compiler.phases.typing.UnifyTypes.unify
import evolution.compiler.phases.typing.config.{ Constant1, Constant2, TypingConfig }
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.tree.TreeF._
import evolution.compiler.tree._

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
            Qualified(TypeT.Double =>: TypeT.Double =>: TypeT.Point)
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
          val allConstraints = constraints.merge(Constraints(evolution.annotation.value -> TypeT.Evo(TypeT.Point)))
          val unifier = unify(allConstraints)
          unifier.map(_.substitution.substitute(evolution.annotation)) shouldBe Right(
            Qualified(TypeT.Evo(TypeT.Point))
          )
        }
      }

      "point expressions" in {
        val untyped =
          App.of(Identifier.const(Constant2.Point).embed, Identifier("a").embed, Identifier("b").embed).embed
        val extraBindings = new TypeBindings(
          Map(
            "a" -> TypeBinding.Fixed("a", Qualified(TypeT.Double)),
            "b" -> TypeBinding.Fixed("b", Qualified(TypeT.Double))
          )
        )
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped, extraBindings).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).annotation.value shouldBe TypeT.Point
      }

      "app(x -> x, 2)" in {
        val identity = Lambda("x", Identifier("x").embed).embed
        val untyped = App.of(identity, DoubleLiteral(2).embed).embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).annotation.value shouldBe TypeT.Double
      }

      "const(1)" in {
        val untyped = App.of(Identifier.const(Constant1.Constant).embed, DoubleLiteral(1).embed).embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val finalExpr = substitution.substitute(expr)
        finalExpr.annotation.value shouldBe TypeT.Evo(TypeT.Double)

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
        substitution.substitute(expr).annotation.value shouldBe TypeT.Evo(TypeT.Point)
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
        typedExpr.annotation.value shouldBe TypeT.Evo(TypeT.Point)
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
        typedExpr.annotation.value shouldBe TypeT.Evo(TypeT.Point)
      }
    }
  }

  def assignVarsAndFindConstraints(
    expr: Tree,
    extraTypeBindings: TypeBindings = TypeBindings.empty
  ): Either[String, (TypedTree, Constraints)] = {

    val exprWithVars = AssignFreshTypeVars.assign(expr, TypingConfig.constantQualifiedTypes.merge(extraTypeBindings))

    for {
      constraints <- FindConstraints.find(exprWithVars)
    } yield (exprWithVars, constraints)
  }
}