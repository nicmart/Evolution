package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term.PArg.{PInst, PVar}
import evolution.compiler.term.Term.{PApp, PArg}
import evolution.compiler.tree.AnnotatedTree
import evolution.compiler.tree.AnnotatedTree.AwaitingAnnotation
import evolution.compiler.tree.TypedTree._
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClassInstance.NumericInst
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

class TreeToTermCompilerTest extends LanguageSpec {

  "TreeToTermCompiler" - {
    "should compile" - {
      "literals" - {

        "bools" in {
          val tree = Bool(true).as(Type.Bool)
          val term = compiler.compile(tree).unsafeRight

          term shouldBe Term.Lit(LitBool(true))
        }

        "doubles" in {
          val tree = DoubleLiteral(0).as(Type.Double)
          val term = compiler.compile(tree).unsafeRight

          term shouldBe Term.Lit(LitDouble(0))
        }

        "integers" - {
          "polymorphic" in {
            val predicate = Predicate("Num", List(Type.Var("X")))
            val tree = IntLiteral(0).as(Type.Var("X"), predicate)
            val state = CompilerState.empty.withPred(predicate)
            val predVarName = state.predName(predicate).get

            val term = compiler.compileM(tree).run(state).unsafeRight

            term shouldBe Term.PApp(Term.Lit(LitInt(0)), PArg.PVar(predVarName))
          }

          "monomorphic" in {
            val predicate = Predicate("Num", List(Type.Double))
            val tree = IntLiteral(0).as(Type.Double, predicate)
            val instance = NumericInst(TypingConfig.numeric(Type.Double).unsafeRight)

            val term = compiler.compile(tree).unsafeRight

            term shouldBe Term.PApp(Term.Lit(LitInt(0)), PArg.PInst(instance))
          }
        }
      }

      "identifiers" - {
        "unqualified" in {
          val tree = Id("x").as(Type.Double)
          val term = compiler.compile(tree).unsafeRight

          term shouldBe Term.Id("x")
        }

        "qualified with non-resolved predicates" in {
          val predicate = Predicate("MyPred", List(Type.Var("X"), Type.Double))
          val tree = Id("x").as(Type.Var("X"), predicate)
          val state = CompilerState.empty.withPred(predicate)
          val term = compiler.compileM(tree).run(state).unsafeRight

          term shouldBe PApp(Term.Id("x"), PVar(state.predName(predicate).get))
        }

        "qualified with resolved predicate" in {
          val predicate = Predicate("Num", List(Type.Double))
          val tree = Id("x").as(Type.Double, predicate)
          val state = CompilerState.empty.withPred(predicate)
          val term = compiler.compileM(tree).run(state).unsafeRight

          term shouldBe PApp(Term.Id("x"), PInst(TypingConfig.instance(predicate).unsafeRight))
        }

        "qualified with mixed predicates" in {
          val predicate1 = Predicate("MyPred", List(Type.Var("X"), Type.Double))
          val predicate2 = Predicate("Num", List(Type.Double))
          val tree = Id("x").as(Type.Var("X"), predicate1, predicate2)
          val state = CompilerState.empty.withPred(predicate1)
          val term = compiler.compileM(tree).run(state).unsafeRight

          term shouldBe PApp(
            PApp(Term.Id("x"), PVar(state.predName(predicate1).get)),
            PInst(TypingConfig.instance(predicate2).unsafeRight)
          )
        }
      }

    }
  }

  lazy val compiler = new TreeToTermCompiler

  private implicit class AwaitingAnnotationOps(awaitingAnnotation: AwaitingAnnotation[Qualified[Type]]) {
    def as(tpe: Type, predicates: Predicate*): AnnotatedTree[Qualified[Type]] =
      awaitingAnnotation.as(Qualified(predicates.toList, tpe))
  }
}
