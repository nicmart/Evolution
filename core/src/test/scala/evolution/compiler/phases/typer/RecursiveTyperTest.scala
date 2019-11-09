package evolution.compiler.phases.typer

import cats.data.NonEmptyList
import evolution.compiler.module.Module
import evolution.compiler.phases.typer.RecursiveTyper.InferenceState
import evolution.compiler.tree.TreeF
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.{ Predicate, Qualified }
import evolution.compiler.types.{ Assumption, Assumptions, Type }
import org.scalatest.{ EitherValues, FreeSpec, Matchers }
import evolution.compiler.tree.TreeF._

class RecursiveTyperTest extends FreeSpec with Matchers with EitherValues {

  "RecursiveTyperTest should type" - {
    "integer literals" in {
      val untyped = IntLiteral(1)
      val state = InferenceState.empty
      val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
      val currentTypeVar = Type.Var(state.currentTypeVarname)
      typed.right.value shouldBe untyped.annotate(
        Qualified(List(Predicate("Num", List(currentTypeVar))), currentTypeVar)
      )
    }

    "double literals" in {
      val untyped = DoubleLiteral(2.1)
      val typed = typer.typeTree(untyped.embed, None, Module.empty)
      typed.right.value shouldBe untyped.annotate(Qualified(Type.Double))
    }

    "booleans" in {
      val untyped = Bool(true)
      val typed = typer.typeTree(untyped.embed, None, Module.empty)
      typed.right.value shouldBe untyped.annotate(Qualified(Type.Bool))
    }

    "identifiers" - {
      "fixed type" in {
        val untyped = Identifier("x")
        val state = stateWithAssumptions(Assumption("x", Qualified(Scheme(Type.Var("T1"))), false))
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.right.value shouldBe untyped.annotate(Qualified(Type.Var("T1")))
      }

      "schema type" in {
        val untyped = Identifier("x")
        val state = stateWithAssumptions(Assumption("x", Qualified(Scheme(List("Y"), Type.Var("Y"))), false))
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.right.value shouldBe untyped.annotate(Qualified(state.currentTypeVar))
      }

      "undefined type" in {
        val untyped = Identifier("x")
        val state = InferenceState.empty
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.isLeft shouldBe true
      }
    }

    "lambdas" - {
      "identity" in {
        val untyped = Lambda("x", Identifier("x").embed)
        val state = InferenceState.empty
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.right.value shouldBe TreeF
          .Lambda("x", Identifier("x").annotate(Qualified(state.currentTypeVar)))
          .annotate(Qualified[Type](state.currentTypeVar =>: state.currentTypeVar))
      }

      "use existing assumptions for other identifiers" in {
        val untyped = Lambda("x", Identifier("y").embed)
        val yPredicates = List(Predicate("MyPred", List(Type.Var("Y"))))
        val yQualifiedType = Qualified[Type](yPredicates, Type.Var("Y"))
        val yAssumption = Assumption("y", yQualifiedType.map(Scheme.apply), false)
        val state = stateWithAssumptions(yAssumption)
        val typed = typer.typeTreeF(untyped.embed, None, Module.empty).runA(state)
        typed.right.value shouldBe TreeF
          .Lambda("x", Identifier("y").annotate(yQualifiedType))
          .annotate(Qualified[Type](yPredicates, state.currentTypeVar =>: Type.Var("Y")))
      }

      "new assumption shadows existing one" in {
        val untyped = Lambda("x", Identifier("x").embed).embed
        val assumptionThatWillBeShadowed = Assumption("x", Qualified(Scheme(Type.Double)), false)
        val state = stateWithAssumptions(assumptionThatWillBeShadowed)
        val typed = typer.typeTreeF(untyped, None, Module.empty).runA(state)
        typed.right.value shouldBe TreeF
          .Lambda("x", Identifier("x").annotate(Qualified(state.currentTypeVar)))
          .annotate(Qualified[Type](state.currentTypeVar =>: state.currentTypeVar))
      }
    }

    "app" - {
      "app(f: X -> Double, x: Bool): Double" in {
        val untyped = App(Identifier("f").embed, NonEmptyList.of(Identifier("x").embed)).embed
        val state = stateWithAssumptions(
          Assumption("f", Qualified(Scheme(Type.Var("X") =>: Type.Double)), false),
          Assumption("x", Qualified(Scheme(Type.Bool)), false)
        )
        val typed = typer.typeTreeAndSubstitute(untyped, None, Module.empty).runA(state).right.value
        typed.annotation.value shouldBe Type.Double
      }
    }
  }

  def stateWithAssumptions(assumptions: Assumption*): InferenceState =
    InferenceState.empty.withAssumptions(assumptions.foldLeft(Assumptions.empty) {
      case (ass, a) => ass.withAssumption(a)
    })

  val typer = new RecursiveTyper
}
