package evolution.compiler.phases.typer

import evolution.compiler.module.Module
import evolution.compiler.phases.typer.RecursiveTyper.InferenceState
import evolution.compiler.tree.TreeF
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.{ Predicate, Qualified }
import evolution.compiler.types.{ Assumption, Assumptions, Type }
import org.scalatest.{ EitherValues, FreeSpec, Matchers }

class RecursiveTyperTest extends FreeSpec with Matchers with EitherValues {

  "RecursiveTyperTest should type" - {
    "integer literals" in {
      val untyped = TreeF.IntLiteral(1)
      val state = InferenceState.empty
      val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
      typed.right.value shouldBe untyped.annotate(
        Qualified(List(Predicate("Num", List(state.currentTypeVar))), state.currentTypeVar)
      )
    }

    "double literals" in {
      val untyped = TreeF.DoubleLiteral(2.1)
      val typed = typer.typeTree(untyped.embed, None, Module.empty)
      typed.right.value shouldBe untyped.annotate(Qualified(Type.Double))
    }

    "booleans" in {
      val untyped = TreeF.Bool(true)
      val typed = typer.typeTree(untyped.embed, None, Module.empty)
      typed.right.value shouldBe untyped.annotate(Qualified(Type.Bool))
    }

    "identifiers" - {
      "fixed type" in {
        val untyped = TreeF.Identifier("x")
        val assumption = Assumptions.empty.withAssumption(Assumption("x", Qualified(Scheme(Type.Var("T1"))), false))
        val state = InferenceState.empty.withAssumptions(assumption)
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.right.value shouldBe untyped.annotate(Qualified(Type.Var("T1")))
      }

      "schema type" in {
        val untyped = TreeF.Identifier("x")
        val assumption =
          Assumptions.empty.withAssumption(Assumption("x", Qualified(Scheme(List("Y"), Type.Var("Y"))), false))
        val state = InferenceState.empty.withAssumptions(assumption)
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.right.value shouldBe untyped.annotate(Qualified(state.currentTypeVar))
      }

      "undefined type" in {
        val untyped = TreeF.Identifier("x")
        val state = InferenceState.empty
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.isLeft shouldBe true
      }
    }
  }

  val typer = new RecursiveTyper
}
