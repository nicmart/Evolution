package evolution.compiler.term
import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypeclassConfig
import evolution.compiler.term.Term.Literal.*
import evolution.compiler.tree.AnnotatedTree
import evolution.compiler.tree.AnnotatedTree.AwaitingAnnotation
import evolution.compiler.tree.TypedTree.*
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}
import evolution.compiler.types.TypeClasses

class TreeToTermCompilerTest extends LanguageSpec:
  "TreeToTermCompiler" - {
    "should compile" - {
      "literals" - {

        "bools" in {
          val tree = Bool(true).as(Type.Bool)
          val term = compiler.compile(tree).unsafeRight
          term `shouldBe` Term.Lit(LitBool(true))
        }

        "doubles" in {
          val tree = DoubleLiteral(0).as(Type.Double)
          val term = compiler.compile(tree).unsafeRight

          term `shouldBe` Term.Lit(LitDouble(0))
        }

        "integers" - {
          "polymorphic" in {
            val predicate = Predicate("Num", List(Type.Var("X")))
            val tree = IntLiteral(0).as(Type.Var("X"), predicate)
            val state = CompilerState.empty.withPredicate(predicate)
            val predVarName = state.predName(predicate).get

            val term = compiler.compileM(tree).run(state).unsafeRight

            term `shouldBe` Term.Apply(Term.Lit(LitInt(0)), Term.Id(predVarName))
          }

          "monomorphic" in {
            val predicate = Predicate("Num", List(Type.Double))
            val tree = IntLiteral(0).as(Type.Double, predicate)
            val instance = TypeclassConfig.instance(TypeclassConfig.TypeClassId.Num, Type.Double).unsafeRight
            val term = compiler.compile(tree).unsafeRight

            term `shouldBe` Term.Apply(Term.Lit(LitInt(0)), Term.Inst(instance))
          }
        }
      }

      "identifiers" - {
        "unqualified" in {
          val tree = Id("x").as(Type.Double)
          val term = compiler.compile(tree).unsafeRight

          term `shouldBe` Term.Id("x")
        }

        "qualified with non-resolved predicates" in {
          val predicate = Predicate("MyPred", List(Type.Var("X"), Type.Double))
          val tree = Id("x").as(Type.Var("X"), predicate)
          val state = CompilerState.empty.withPredicate(predicate)
          val term = compiler.compileM(tree).run(state).unsafeRight

          term `shouldBe` Term.Apply(Term.Id("x"), Term.Id(state.predName(predicate).get))
        }

        "qualified with resolved predicate" in {
          val predicate = Predicate("Num", List(Type.Double))
          val tree = Id("x").as(Type.Double, predicate)
          val state = CompilerState.empty.withPredicate(predicate)
          val term = compiler.compileM(tree).run(state).unsafeRight

          term `shouldBe` Term.Apply(Term.Id("x"), Term.Inst(predicate.instance))
        }

        "qualified with mixed predicates" in {
          val predicate1 = Predicate("MyPred", List(Type.Var("X"), Type.Double))
          val predicate2 = Predicate("Num", List(Type.Double))
          val tree = Id("x").as(Type.Var("X"), predicate1, predicate2)
          val state = CompilerState.empty.withPredicate(predicate1)
          val term = compiler.compileM(tree).run(state).unsafeRight

          term `shouldBe` Term.Apply(
            Term.Apply(Term.Id("x"), Term.Id(state.predName(predicate1).get)),
            Term.Inst(predicate2.instance)
          )
        }
      }

      "lets" - {
        "monomorphic: x = true in x" in {
          val tree = Let(
            "x",
            Bool(true).as(Type.Bool),
            Id("x").as(Type.Bool)
          ).as(Type.Bool)

          val term = compiler.compile(tree).unsafeRight

          term `shouldBe` Term.Let("x", Term.Lit(LitBool(true)), Term.Id("x"))
        }

        "polymorphic: x = (2: Num(T) => T) in (x: Double)" in {
          val predicate = Predicate("Num", List(Type.Var("T")))
          val predicateInst = Predicate("Num", List(Type.Double))

          val tree = Let(
            "x",
            IntLiteral(2).as(Type.Var("T"), predicate),
            Id("x").as(Type.Double, predicateInst)
          ).as(Type.Double)

          val pVarName = CompilerState.empty.withPredicate(predicate).predName(predicate).get

          val term = compiler.compile(tree).unsafeRight

          val expected =
            Term.Let(
              "x",
              Term.Lambda(pVarName, Term.Apply(Term.Lit(LitInt(2)), Term.Id(pVarName))),
              Term.Apply(Term.Id("x"), Term.Inst(predicateInst.instance))
            )

          term `shouldBe` expected
        }
      }

      "lambdas" - {
        "identity" in {
          val tree = Lambda("x", Id("x").as(Type.Var("T"))).as(Type.Var("T") =>: Type.Var("T"))
          val term = compiler.compile(tree).unsafeRight

          term `shouldBe` Term.Lambda("x", Term.Id("x"))
        }
      }

      "applications" - {
        "of identifiers" in {
          val tree =
            App
              .of(
                Id("x").as(Type.Var("X") =>: Type.Var("X") =>: Type.Var("X")),
                IntLiteral(1).as(Type.Var("X")),
                IntLiteral(2).as(Type.Var("X"))
              )
              .as(Type.Var("X"))
          val term = compiler.compile(tree).unsafeRight

          term `shouldBe` Term.Apply(Term.Apply(Term.Id("x"), Term.Lit(LitInt(1))), Term.Lit(LitInt(2)))
        }
      }
    }
  }

  lazy val compiler = TreeToTermCompiler()

  extension (awaitingAnnotation: AwaitingAnnotation[Qualified[Type]])
    def as(tpe: Type, predicates: Predicate*): AnnotatedTree[Qualified[Type]] =
      awaitingAnnotation.as(Qualified(predicates.toList, tpe))

  extension (predicate: Predicate) def instance: TypeClassInstance = TypeclassConfig.instance(predicate).unsafeRight
