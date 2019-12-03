package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term.PArg
import evolution.compiler.tree.TypedTree._
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.{Predicate, Qualified}

class TreeToTermCompilerTest extends LanguageSpec {

  "TreeToTermCompiler" - {
    "should compile" - {
      "literals" - {

        "bools" in {
          val tree = Bool(true).as(Qualified(Type.Bool))
          val term = compiler.compile(tree).unsafeRight

          term shouldBe Term.Lit(LitBool(true))
        }

        "doubles" in {
          val tree = DoubleLiteral(0).as(Qualified(Type.Double))
          val term = compiler.compile(tree).unsafeRight

          term shouldBe Term.Lit(LitDouble(0))
        }

        "integers" - {
          "polymorphic" in {
            val predicate = Predicate("Num", List(Type.Var("X")))
            val qualifiedType = Qualified(List(predicate), Type.Var("X"))
            val tree = IntLiteral(0).as(qualifiedType)
            val state = CompilerState.empty.withPred(predicate)
            val predVarName = state.predName(predicate).get

            val term = compiler.compileM(tree).run(state).unsafeRight

            term shouldBe Term.PApp(Term.Lit(LitInt(0)), PArg.PVar(predVarName))
          }
        }
      }
    }
  }

  lazy val compiler = new TreeToTermCompiler
}
