package evolution.compiler.phases.typer

import evolution.compiler.LanguageSpec
import evolution.compiler.tree.TreeF.{App, Identifier, Lambda, Let}
import evolution.compiler.types.Type.Scheme
import evolution.compiler.tree.TreeF._
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
            Lambda("b", App.of(Identifier("mult").embed, Identifier("a").embed, Identifier("b").embed).embed).embed
          ).embed,
          Identifier("f").embed
        ).embed
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
