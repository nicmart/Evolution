package evolution.compiler.phases.typer

import evolution.compiler.LanguageSpec
import evolution.compiler.types.Type.Scheme
import evolution.compiler.tree.Tree._
import evolution.compiler.types.{Assumption, Assumptions, Type}
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.logging.{ColorPPrinterLogger, NoOpLogger}

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
      println(typed)
      println(typed.annotation)
      typed.annotation.value shouldBe x =>: y =>: z
    }

    "f(a, b) = a + (-b) in f" in {
      val addScheme = Scheme(List("A", "B", "C"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("C"))
      val addPredicates = List(Predicate("Add", List(Type.Var("A"), Type.Var("B"), Type.Var("C"))))

      val invScheme = Scheme(List("A"), Type.Var("A") =>: Type.Var("A"))
      val invPredicates = List(Predicate("Inv", List(Type.Var("A"))))

      val assumptions = withAssumptions(
        Assumption("add", Qualified(addPredicates, addScheme), true),
        Assumption("inv", Qualified(invPredicates, invScheme), true)
      )

      val untyped =
        Let(
          "f",
          Lambda(
            "a",
            Lambda("b", App.of(Id("add"), Id("a"), App.of(Id("inv"), Id("b"))))
          ),
          Id("f")
        )

      val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
      val List(Predicate("Mult", List(x, y, z))) =
        typed.annotation.predicates.distinct
      println(typed)
      println(typed.annotation)
      typed.annotation.value shouldBe x =>: y =>: z
    }
  }

  def withAssumptions(assumptions: Assumption*): Assumptions =
    assumptions.foldLeft(Assumptions.empty) {
      case (ass, a) => ass.withAssumption(a)
    }

  //lazy val typer = new RecursiveTyper
  lazy val typer = new PredicatesSolverTyper(new RecursiveTyper, new UnifyPredicates(ColorPPrinterLogger))
}
