package evolution.compiler.phases.typer

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.model.{Assumption, Assumptions}
import evolution.compiler.tree.Tree._
import evolution.compiler.tree.{PrettyPrintTypedTree, TypedTree => T}
import evolution.compiler.types.Type
import evolution.compiler.types.Type.Scheme
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

class RecursiveTyperTest extends LanguageSpec:

  "RecursiveTyperTest" - {
    "expressions" - {
      "integer literals" in {
        val untyped = IntLiteral(1)
        val typed = typer.typeTree(untyped, None, Assumptions.empty).unsafeRight
        val Qualified(_, Type.Var(varname)) = typed.annotation
        val expectedType = Qualified(List(Predicate("Num", List(Type.Var(varname)))), Type.Var(varname))
        val expected = T.IntLiteral(1).as(expectedType)
        typed shouldBe expected
      }

      "double literals" in {
        val untyped = DoubleLiteral(2.1)
        val typed = typer.typeTree(untyped, None, Assumptions.empty)
        val expected = T.DoubleLiteral(2.1).as(Qualified(Type.Double))
        typed.unsafeRight shouldBe expected
      }

      "booleans" in {
        val untyped = Bool(true)
        val typed = typer.typeTree(untyped, None, Assumptions.empty)
        typed.unsafeRight shouldBe T.Bool(true).as(Qualified(Type.Bool))
      }

      "identifiers" - {
        "fixed type" in {
          val untyped = Id("x")
          val assumptions = withAssumptions(Assumption("x", Qualified(Scheme(Type.Var("T1")))))
          val typed = typer.typeTree(untyped, None, assumptions)
          typed.unsafeRight shouldBe T.Id("x").as(Qualified(Type.Var("T1")))
        }

        "schema type" in {
          val untyped = Id("x")
          val assumptions = withAssumptions(model.Assumption("x", Qualified(Scheme(List("Y"), Type.Var("Y")))))
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          val Type.Var(varname) = typed.annotation.value
          typed shouldBe T.Id("x").as(Qualified(Type.Var(varname)))
        }

        "undefined type" in {
          val untyped = Id("x")
          typer.typeTree(untyped, None, Assumptions.empty).unsafeLeft
        }
      }

      "lambdas" - {
        "identity" in {
          val untyped = Lambda("x", Id("x"))
          val typed = typer.typeTree(untyped, None, Assumptions.empty).unsafeRight
          val Type.Arrow(Type.Var(varname), Type.Var(varname2)) = typed.annotation.value
          varname shouldBe varname2
          val expectedType = Qualified[Type](Type.Var(varname) =>: Type.Var(varname))
          val expected =
            T.Lambda("x", T.Id("x").as(Qualified[Type](Type.Var(varname)))).as(expectedType)
          typed shouldBe expected
        }

        "use existing assumptions for other identifiers" in {
          val untyped = Lambda("x", Id("y"))
          val yPredicates = List(Predicate("MyPred", List(Type.Var("Y"))))
          val yQualifiedType = Qualified[Type](yPredicates, Type.Var("Y"))
          val yAssumption = model.Assumption("y", yQualifiedType.map(Scheme.apply(_)))
          val typed = typer.typeTree(untyped, None, withAssumptions(yAssumption)).unsafeRight
          val Type.Arrow(Type.Var(varname), Type.Var("Y")) = typed.annotation.value
          val expectedType = Qualified[Type](yPredicates, Type.Var(varname) =>: Type.Var("Y"))
          val expected = T.Lambda("x", T.Id("y").as(yQualifiedType)).as(expectedType)
          typed shouldBe expected
        }

        "new assumption shadows existing one" in {
          val untyped = Lambda("x", Id("x"))
          val assumptionThatWillBeShadowed = model.Assumption("x", Qualified(Scheme(Type.Double)))
          val typed = typer.typeTree(untyped, None, withAssumptions(assumptionThatWillBeShadowed)).unsafeRight
          val Type.Arrow(Type.Var(var1), Type.Var(var2)) = typed.annotation.value
          var1 shouldBe var2
          val expectedType = Qualified[Type](Type.Var(var1) =>: Type.Var(var1))
          val expected = T.Lambda("x", T.Id("x").as(Qualified[Type](Type.Var(var1)))).as(expectedType)
          typed shouldBe expected
        }
      }

      "app" - {
        "app(f: X -> Double, x: Bool): Double" in {
          val untyped = App.of(Id("f"), Id("x"))
          val assumptions = withAssumptions(
            model.Assumption("f", Qualified(Scheme(Type.Var("X") =>: Type.Double))),
            model.Assumption("x", Qualified(Scheme(Type.Bool)))
          )
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          typed.annotation.value shouldBe Type.Double
        }

        "app(f: X -> Y -> Double, x: Bool, y: Double): Double" in {
          val untyped = App
            .of(
              Id("f"),
              Id("x"),
              Id("y")
            )

          val assumptions = withAssumptions(
            model.Assumption("f", Qualified(Scheme(Type.Var("X") =>: Type.Var("Y1") =>: Type.Double))),
            model.Assumption("x", Qualified(Scheme(Type.Bool))),
            model.Assumption("y", Qualified(Scheme(Type.Var("Y2"))))
          )
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          typed.annotation.value shouldBe Type.Double
        }
      }

      "lets" - {
        "f(x) = x in f(2)" in {
          val untyped =
            Let("f", Lambda("x", Id("x")), App.of(Id("f"), IntLiteral(2)))
          val typed = typer.typeTree(untyped, None, Assumptions.empty).unsafeRight
          val Qualified(predicates, Type.Var(t)) = typed.annotation
          predicates should contain only Predicate("Num", List(Type.Var(t)))
        }
      }

      "lists" - {
        "list(a: Double, b: X): List[Double]" in {
          val untyped = Lst(List(Id("a"), Id("b")))
          val assumptions = withAssumptions(
            model.Assumption("a", Qualified(Scheme(Type.Double))),
            model.Assumption("b", Qualified(Scheme(Type.Var("X"))))
          )
          val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
          typed.annotation.value shouldBe Type.Lst(Type.Double)
        }
      }
    }

    "expected types" - {
      "successful check" in {
        val assumptions = withAssumptions(
          model.Assumption("x", Qualified(List(Predicate("MyPred", List(Type.Var("X")))), Scheme(Type.Var("X"))))
        )
        val untyped = Id("x")
        val typed = typer.typeTree(untyped, Some(Type.Double), assumptions).unsafeRight
        typed.annotation.predicates should contain only Predicate("MyPred", List(Type.Double))
        typed.annotation.value shouldBe Type.Double
      }

      "failing check" in {
        val assumptions = withAssumptions(
          model.Assumption("x", Qualified(Scheme(Type.Double)))
        )
        val untyped = Id("x")
        typer.typeTree(untyped, Some(Type.Integer), assumptions).unsafeLeft
      }
    }

    "functional tests" - {
      "1 + 1" in {
        val addScheme = Scheme(List("A", "B", "C"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("C"))
        val addPredicates = List(Predicate("Add", List(Type.Var("A"), Type.Var("B"), Type.Var("C"))))
        val assumptions = withAssumptions(model.Assumption("add", Qualified(addPredicates, addScheme)))
        val untyped = App.of(Id("add"), IntLiteral(1), IntLiteral(1))
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        val List(Predicate("Add", List(x, y, z)), Predicate("Num", List(xx)), Predicate("Num", List(yy))) =
          typed.annotation.predicates
        x shouldBe xx
        y shouldBe yy
        z shouldBe typed.annotation.value
      }

      "x = f in x" in {
        val scheme = Scheme(List("A"), Type.Var("A"))
        val predicates = List(Predicate("Hei", List(Type.Var("A"))))
        val assumptions = withAssumptions(Assumption("f", Qualified(predicates, scheme)))
        val untyped = Let("x", Id("f"), Id("x"))
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        val List(Predicate("Hei", List(Type.Var(varName)))) = typed.annotation.predicates
        typed.annotation.value shouldBe Type.Var(varName)
      }

      "f(f(1.0)): Double where f: forall X. X => X" in {
        val assumptions = withAssumptions(
          model.Assumption("f", Qualified(Scheme(List("X"), Type.Var("X") =>: Type.Var("X"))))
        )
        val untyped = App.of(Id("f"), App.of(Id("f"), DoubleLiteral(1)))
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        typed.annotation.value shouldBe Type.Double
      }

      "f(a, b) = a * b in f" in {
        val multScheme = Scheme(List("A", "B", "C"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("C"))
        val addPredicates = List(Predicate("Mult", List(Type.Var("A"), Type.Var("B"), Type.Var("C"))))
        val assumptions = withAssumptions(model.Assumption("mult", Qualified(addPredicates, multScheme)))
        val untyped =
          Let("f", Lambda("a", Lambda("b", App.of(Id("mult"), Id("a"), Id("b")))), Id("f"))
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        val List(Predicate("Mult", List(x, y, z))) = typed.annotation.predicates.distinct
        typed.annotation.value shouldBe x =>: y =>: z
      }

      "f(a, b) = a + (-b) in f" in {
        val addScheme = Scheme(List("A", "B", "C"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("C"))
        val addPredicates = List(Predicate("Add", List(Type.Var("A"), Type.Var("B"), Type.Var("C"))))

        val invScheme = Scheme(List("A"), Type.Var("A") =>: Type.Var("A"))
        val invPredicates = List(Predicate("Inv", List(Type.Var("A"))))

        val assumptions = withAssumptions(
          model.Assumption("add", Qualified(addPredicates, addScheme)),
          model.Assumption("inv", Qualified(invPredicates, invScheme))
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
        val List(
          Predicate("Add", List(Type.Var(var1), Type.Var(var2), Type.Var(var3))),
          Predicate("Inv", List(Type.Var(var4)))
        ) = typed.annotation.predicates.distinct

        var2 shouldBe var4
        typed.annotation.value shouldBe Type.Var(var1) =>: Type.Var(var2) =>: Type.Var(var3)
      }

      "f(1.1, 2.1)" in {
        val scheme = Scheme(List("A", "B"), Type.Var("A") =>: Type.Var("B") =>: Type.Var("A"))

        val assumptions = withAssumptions(
          model.Assumption("f", Qualified(scheme))
        )

        val untyped =
          App.of(Id("f"), DoubleLiteral(1.1), DoubleLiteral(2.2))

        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        typed.annotation.value shouldBe Type.Double
      }

      "f = a in f" in {
        val scheme = Scheme(List("A"), Type.Var("A"))
        val predicates = List(Predicate("Num", List(Type.Var("A"))))
        val assumptions = withAssumptions(Assumption("a", Qualified(predicates, scheme)))
        val untyped = Let("f", Id("a"), Id("f"))
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        println(PrettyPrintTypedTree(typed))
        val List(Predicate("Num", List(x))) = typed.annotation.predicates.distinct
        typed.annotation.value shouldBe x
      }

      "f(a) = neg(a) in f" in {
        val scheme = Scheme(List("A", "B"), Type.Var("A") =>: Type.Var("B"))
        val predicates = List(Predicate("Neg", List(Type.Var("A"), Type.Var("B"))))
        val assumptions = withAssumptions(Assumption("neg", Qualified(predicates, scheme)))
        val untyped = Let("f", Lambda("a", App.of(Id("neg"), Id("a"))), Id("f"))
        val typed = typer.typeTree(untyped, None, assumptions).unsafeRight
        println(PrettyPrintTypedTree(typed))
        val List(Predicate("Neg", List(x, y))) =
          typed.annotation.predicates.distinct
        typed.annotation.value shouldBe x =>: y
      }

    }
  }
  def withAssumptions(assumptions: Assumption*): Assumptions =
    assumptions.foldLeft(Assumptions.empty) {
      case (ass, a) => ass.withAssumption(a)
    }

  lazy val typer = new RecursiveTyper
