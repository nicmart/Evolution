package evolution.compiler
import evolution.compiler.types._
import evolution.compiler.types.TypeClasses._
import evolution.compiler.phases.typing.FindConstraints
import evolution.compiler.phases.typing.AssignFreshTypeVars
import evolution.compiler.phases.typing.config.TypingConfig
import evolution.compiler.phases.typing.model.Constraints
import evolution.compiler.tree.TreeF._
import evolution.compiler.tree.Tree
import evolution.compiler.phases.typing.model.TypeVarGenerator

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
          val expected = Constraints(firstTypeVar -> Type.Dbl)
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
