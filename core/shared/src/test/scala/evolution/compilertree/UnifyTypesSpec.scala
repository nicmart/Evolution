package evolution.compilertree

import evolution.compilertree.types._
import evolution.compilertree.types.TypeClasses._
import evolution.compilertree.phases.typing.FindConstraints
import evolution.compilertree.phases.typing.AssignFreshTypeVars
import evolution.compilertree.phases.typing.UnifyTypes.unify
import evolution.compilertree.phases.typing.config.{ Constant0, Constant1, Constant2, TypingConfig }
import evolution.compilertree.phases.typing.model.Constraints
import evolution.compilertree.ast.TreeF._

class UnifyTypesSpec extends LanguageSpec {

  "UnifyTypes" - {

    "should unify" - {

      "pre-defined constants" - {
        "point function" in {
          val point =
            AssignFreshTypeVars.assign(Const(Constant2.Point), TypingConfig.constantQualifiedTypes)
          val constraints = FindConstraints.find(point).unsafeEvaluate
          val unifier = unify(constraints)
          unifier.map(_.substitution.substitute(point.value)) shouldEq Right(
            Qualified(Type.Dbl =>: Type.Dbl =>: Type.Point)
          )
        }

        "constant evolution of a point" in {
          val point = AppN(Const(Constant2.Point), DoubleLiteral(1).embed, DoubleLiteral(1).embed)
          val evolution =
            AssignFreshTypeVars.assign(
              App(Const(Constant1.Constant), point).embed,
              TypingConfig.constantQualifiedTypes
            )
          val constraints = FindConstraints.find(evolution).unsafeEvaluate
          val allConstraints = constraints.merge(Constraints(evolution.value.value -> Type.Evo(Type.Point)))
          val unifier = unify(allConstraints)
          unifier.map(_.substitution.substitute(evolution.value)) shouldEq Right(
            Qualified(Type.Evo(Type.Point))
          )
        }
      }

      "point expressions" in {
        val untyped = AppN(Const(Constant2.Point), Identifier("a").embed, Identifier("b").embed)
        val extraBindings = new TypeBindings(
          Map(
            "a" -> TypeBinding.Fixed("a", Qualified(Type.Dbl)),
            "b" -> TypeBinding.Fixed("b", Qualified(Type.Dbl))
          )
        )
        val (expr, constraints) =
          assignVarsAndFindConstraints(untyped, extraBindings).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).value.value shouldEq Type.Point
      }

      "app(x -> x, 2)" in {
        val identity = Lambda("x", Identifier("x").embed).embed
        val untyped = App(identity, DoubleLiteral(2).embed).embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).value.value shouldEq Type.Dbl
      }

      "mapCons(empty, head -> tail -> cons(1, tail))" in {
        val untyped = AppN(
          Const(Constant2.MapCons),
          Const(Constant0.Empty),
          Lambda(
            "head",
            Lambda(
              "tail",
              AppN(Const(Constant2.Cons), DoubleLiteral(1).embed, Identifier("tail").embed)
            ).embed
          ).embed
        )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).value.value shouldEq Type.Evo(Type.Dbl)
      }

      "const(1)" in {
        val untyped = App(Const(Constant1.Constant), DoubleLiteral(1).embed).embed
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val finalExpr = substitution.substitute(expr)
        finalExpr.value.value shouldEq Type.Evo(Type.Dbl)

        val CoTree(_, App(CoTree(_, Identifier(_, isPrimitive)), _)) = finalExpr
        isPrimitive shouldEq true
      }

      "@point(const(1), const(2))" in {
        val untyped =
          AppN(
            Const(Constant2.LiftedPoint),
            App(Const(Constant1.Constant), DoubleLiteral(1).embed).embed,
            App(Const(Constant1.Constant), DoubleLiteral(2).embed).embed
          )
        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        substitution.substitute(expr).value.value shouldEq Type.Evo(Type.Point)
      }

      "solve1(const(x -> x), point(0, 0))" in {
        val untyped =
          AppN(
            Const(Constant2.Solve1),
            App(Const(Constant1.Constant), Lambda("x", Identifier("x").embed).embed).embed,
            AppN(Const(Constant2.Point), DoubleLiteral(1).embed, DoubleLiteral(2).embed)
          )

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val typedExpr = substitution.substitute(expr)
        typedExpr.value.value shouldEq Type.Evo(Type.Point)
      }

      "uniformChoice(point(1, 2))" in {
        val untyped =
          App(
            Const(Constant1.UniformChoice),
            Lst(List(AppN(Const(Constant2.Point), IntLiteral(1).embed, IntLiteral(2).embed))).embed
          ).embed

        val (expr, constraints) = assignVarsAndFindConstraints(untyped).unsafeEvaluate
        val substitution = unify(constraints).unsafeEvaluate.substitution
        val typedExpr = substitution.substitute(expr)
        typedExpr.value.value shouldEq Type.Evo(Type.Point)
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