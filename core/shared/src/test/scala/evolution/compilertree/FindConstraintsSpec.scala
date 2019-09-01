package evolution.compilertree
import evolution.compilertree.types._
import evolution.compilertree.types.TypeClasses._
import evolution.compilertree.phases.typing.FindConstraints
import evolution.compilertree.phases.typing.AssignFreshTypeVars
import evolution.compilertree.phases.typing.config.TypingConfig
import evolution.compilertree.phases.typing.model.Constraints
import evolution.compilertree.ast.TreeF._
import evolution.compilertree.phases.typing.model.TypeVarGenerator

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