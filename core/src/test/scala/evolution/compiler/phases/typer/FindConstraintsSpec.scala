package evolution.compiler.phases.typer

import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.typer.FindConstraints
import evolution.compiler.phases.typer.AssignFreshTypeVars
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.phases.typer.model.Constraints
import evolution.compiler.tree._
import evolution.compiler.phases.typer.model.TypeVarGenerator
import evolution.compiler.LanguageSpec

class FindConstraintsSpec extends LanguageSpec {

  "FindConstraints" - {
    "should generate constraints for" - {
      "int literals" in {
        forAll(genIntNumber) { numberExpr =>
          val constraints = assignVarsAndFindConstraints(numberExpr).unsafeEvaluate._2
          val expected = Constraints.empty.withPredicate(Predicate("Num", List(firstTypeVar)))
          constraints shouldEq expected
        }
      }

      "double literals" in {
        forAll(genDoubleNotIntNumber) { numberExpr =>
          val constraints = assignVarsAndFindConstraints(numberExpr).unsafeEvaluate._2
          val expected = Constraints(firstTypeVar -> Type.Double)
          constraints shouldEq expected
        }
      }

      "vars" in {
        forAll(genVar) { varExpr =>
          val constraints = assignVarsAndFindConstraints(varExpr).unsafeEvaluate._2
          constraints shouldEq Constraints.empty
        }
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

  val firstTypeVar = TypeVarGenerator.empty.current
  val secondTypeVar = TypeVarGenerator.empty.next.current
}
