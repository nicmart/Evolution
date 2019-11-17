package evolution.compiler.phases.typer.predicates

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.model.{Assumption, Assumptions}
import evolution.compiler.phases.typer.{PredicatesSolverTyper, RecursiveTyper}
import evolution.compiler.tree.Tree._
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.compiler.types.Type
import evolution.logging.NoOpLogger

class PredicatesSolverTyperTest extends LanguageSpec {
  "PredicatesSolverTyper" - {
    "f(a, b) = a * b in f" in {
      val multScheme = Scheme(List("A", "B", "C"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("C"))
      val addPredicates = List(Predicate("Mult", List(Type.Var("A"), Type.Var("B"), Type.Var("C"))))
      val assumptions = withAssumptions(Assumption("mult", Qualified(addPredicates, multScheme), true))
      val untyped =
        Let(
          "f",
          Lambda(
            "a",
            Lambda("b", App.of(Id("mult"), Id("a"), Id("b")))
          ),
          Id("f")
        )
      val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
      val List(Predicate("Mult", List(x, y, z))) =
        typed.annotation.predicates.distinct
      typed.annotation.value shouldBe x =>: y =>: z
    }
  }

  def withAssumptions(assumptions: Assumption*): Assumptions =
    assumptions.foldLeft(Assumptions.empty) {
      case (ass, a) => ass.withAssumption(a)
    }

  //lazy val typer = new RecursiveTyper
  lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(NoOpLogger))
}
