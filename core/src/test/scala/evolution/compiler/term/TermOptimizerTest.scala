package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.types.TypeClassInstance.NumericInst
import Term._
import Literal._
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.compiler.types.TypeClasses.Predicate

class TermOptimizerTest extends LanguageSpec {
  "TermOptimizer should" - {
    "optimize application of literals" - {
      "integers" in {
        optimize(litInt(2)) shouldBe Value(2)
      }
    }

    "optimize sums of constants" - {
      "2 + 3" in {
        val term = App(App(App(Id("add"), Inst(addDouble)), litInt(2)), litInt(3))
        optimize(term) shouldBe Value(5)
      }

      "2 + 3 + 4 + 5" in {
        val term = add(addDouble, add(addDouble, add(addDouble, litInt(2), litInt(3)), litInt(4)), litInt(5))
        optimize(term) shouldBe Value(14)
      }
    }

    "optimize children" - {
      "of lambdas" in {
        val term = Lambda("x", App(Lit(LitInt(2)), Inst(numDouble)))
        optimize(term) shouldBe Lambda("x", Value(2))
      }
    }
  }

  lazy val optimizer = new TermOptimizer(new TermInterpreter)

  def optimize(term: Term): Term = optimizer.optimize(term)
  def litInt(n: Int): Term = App(Lit(LitInt(n)), Inst(numDouble))
  def add(inst: TypeClassInstance, a: Term, b: Term): Term =
    App(App(App(Id("add"), Inst(inst)), a), b)

  lazy val numDouble: TypeClassInstance = TypingConfig.instance(Predicate("Num", List(Type.Double))).unsafeRight
  lazy val addDouble = TypingConfig.instance(Predicate("Add", List(Type.Double, Type.Double, Type.Double))).unsafeRight
}
