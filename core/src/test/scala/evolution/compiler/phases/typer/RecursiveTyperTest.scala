package evolution.compiler.phases.typer

import evolution.compiler.LanguageSpec
import evolution.compiler.module.Module
import evolution.compiler.phases.typer.RecursiveTyper.InferenceState
import evolution.compiler.tree.TreeF
import evolution.compiler.tree.TreeF._
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.compiler.types.{Assumption, Assumptions, Type}

class RecursiveTyperTest extends LanguageSpec {

  "RecursiveTyperTest" - {
    "expressions" - {
      "integer literals" in {
        val untyped = IntLiteral(1)
        val typed = typer.typeTree(untyped.embed, None, Assumptions.empty).unsafeRight
        val Qualified(_, Type.Var(varname)) = typed.annotation
        typed shouldBe untyped.annotate(
          Qualified(List(Predicate("Num", List(Type.Var(varname)))), Type.Var(varname))
        )
      }

      "double literals" in {
        val untyped = DoubleLiteral(2.1)
        val typed = typer.typeTree(untyped.embed, None, Assumptions.empty)
        typed.unsafeRight shouldBe untyped.annotate(Qualified(Type.Double))
      }

      "booleans" in {
        val untyped = Bool(true)
        val typed = typer.typeTree(untyped.embed, None, Assumptions.empty)
        typed.unsafeRight shouldBe untyped.annotate(Qualified(Type.Bool))
      }

      "identifiers" - {
        "fixed type" in {
          val untyped = Identifier("x")
          val assumptions = withAssumptions(Assumption("x", Qualified(Scheme(Type.Var("T1"))), false))
          val typed = typer.typeTree(untyped.embed, None, assumptions)
          typed.unsafeRight shouldBe untyped.annotate(Qualified(Type.Var("T1")))
        }

        "schema type" in {
          val untyped = Identifier("x")
          val assumptions = withAssumptions(Assumption("x", Qualified(Scheme(List("Y"), Type.Var("Y"))), false))
          val typed = typer.typeTree(untyped.embed, None, assumptions).unsafeRight
          val Type.Var(varname) = typed.annotation.value
          typed shouldBe untyped.annotate(Qualified(Type.Var(varname)))
        }

        "undefined type" in {
          val untyped = Identifier("x")
          val typed = typer.typeTree(untyped.embed, None, Assumptions.empty).unsafeLeft
        }
      }

      "lambdas" - {
        "identity" in {
          val untyped = Lambda("x", Identifier("x").embed)
          val typed = typer.typeTree(untyped.embed, None, Assumptions.empty).unsafeRight
          val Type.Arrow(Type.Var(varname), Type.Var(varname2)) = typed.annotation.value
          varname shouldBe varname2
          typed shouldBe TreeF
            .Lambda("x", Identifier("x").annotate(Qualified[Type](Type.Var(varname))))
            .annotate(Qualified[Type](Type.Var(varname) =>: Type.Var(varname)))
        }

        "use existing assumptions for other identifiers" in {
          val untyped = Lambda("x", Identifier("y").embed)
          val yPredicates = List(Predicate("MyPred", List(Type.Var("Y"))))
          val yQualifiedType = Qualified[Type](yPredicates, Type.Var("Y"))
          val yAssumption = Assumption("y", yQualifiedType.map(Scheme.apply), false)
          val typed = typer.typeTree(untyped.embed, None, withAssumptions(yAssumption)).unsafeRight
          val Type.Arrow(Type.Var(varname), Type.Var("Y")) = typed.annotation.value
          typed shouldBe TreeF
            .Lambda("x", Identifier("y").annotate(yQualifiedType))
            .annotate(Qualified[Type](yPredicates, Type.Var(varname) =>: Type.Var("Y")))
        }

        "new assumption shadows existing one" in {
          val untyped = Lambda("x", Identifier("x").embed).embed
          val assumptionThatWillBeShadowed = Assumption("x", Qualified(Scheme(Type.Double)), false)
          val typed = typer.typeTree(untyped, None, withAssumptions(assumptionThatWillBeShadowed)).unsafeRight
          val Type.Arrow(Type.Var(varname1), Type.Var(varname2)) = typed.annotation.value
          varname1 shouldBe varname2
          typed shouldBe TreeF
            .Lambda("x", Identifier("x").annotate(Qualified[Type](Type.Var(varname1))))
            .annotate(Qualified[Type](Type.Var(varname1) =>: Type.Var(varname1)))
        }
      }

      "app" - {
        "app(f: X -> Double, x: Bool): Double" in {
          val untyped = App.of(Identifier("f").embed, Identifier("x").embed).embed
          val assumptions = withAssumptions(
            Assumption("f", Qualified(Scheme(Type.Var("X") =>: Type.Double)), false),
            Assumption("x", Qualified(Scheme(Type.Bool)), false)
          )
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          typed.annotation.value shouldBe Type.Double
        }

        "app(f: X -> Y -> Double, x: Bool, y: Double): Double" in {
          val untyped = App
            .of(
              Identifier("f").embed,
              Identifier("x").embed,
              Identifier("y").embed
            )
            .embed

          val assumptions = withAssumptions(
            Assumption("f", Qualified(Scheme(Type.Var("X") =>: Type.Var("Y1") =>: Type.Double)), false),
            Assumption("x", Qualified(Scheme(Type.Bool)), false),
            Assumption("y", Qualified(Scheme(Type.Var("Y2"))), false)
          )
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          typed.annotation.value shouldBe Type.Double
        }
      }

      "lets" - {
        "f(x) = x in f(2)" in {
          val untyped =
            Let("f", Lambda("x", Identifier("x").embed).embed, App.of(Identifier("f").embed, IntLiteral(2).embed).embed).embed
          val typed = typer.typeTree(untyped, None, Assumptions.empty).unsafeRight
          val Qualified(predicates, Type.Var(t)) = typed.annotation
          predicates should contain only Predicate("Num", List(Type.Var(t)))
        }
      }

      "lists" - {
        "list(a: Double, b: X): List[Double]" in {
          val untyped = Lst(List(Identifier("a").embed, Identifier("b").embed)).embed
          val assumptions = withAssumptions(
            Assumption("a", Qualified(Scheme(Type.Double)), false),
            Assumption("b", Qualified(Scheme(Type.Var("X"))), false)
          )
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          typed.annotation.value shouldBe Type.Lst(Type.Double)
        }
      }
    }

    "expected types" - {
      "successful check" in {
        val assumptions = withAssumptions(
          Assumption("x", Qualified(List(Predicate("MyPred", List(Type.Var("X")))), Scheme(Type.Var("X"))), false)
        )
        val untyped = Identifier("x").embed
        val typed = typer.typeTree(untyped, Some(Type.Double), assumptions).unsafeRight
        typed.annotation.predicates should contain only Predicate("MyPred", List(Type.Double))
        typed.annotation.value shouldBe Type.Double
      }

      "failing check" in {
        val assumptions = withAssumptions(
          Assumption("x", Qualified(Scheme(Type.Double)), false)
        )
        val untyped = Identifier("x").embed
        typer.typeTree(untyped, Some(Type.Integer), assumptions).unsafeLeft
      }
    }

    "functional tests" - {
      "1 + 1" in {
        val addScheme = Scheme(List("A"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("C"))
        val addPredicates = List(Predicate("Add", List(Type.Var("A"), Type.Var("B"), Type.Var("C"))))
        val assumptions = withAssumptions(Assumption("add", Qualified(addPredicates, addScheme), true))
        val untyped = App.of(Identifier("add").embed, IntLiteral(1).embed, IntLiteral(1).embed).embed
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        val List(Predicate("Add", List(x, y, z)), Predicate("Num", List(xx)), Predicate("Num", List(yy))) =
          typed.annotation.predicates
        x shouldBe xx
        y shouldBe yy
        z shouldBe typed.annotation.value
      }
    }
  }
  def withAssumptions(assumptions: Assumption*): Assumptions =
    assumptions.foldLeft(Assumptions.empty) {
      case (ass, a) => ass.withAssumption(a)
    }

  lazy val typer = new RecursiveTyper
}
