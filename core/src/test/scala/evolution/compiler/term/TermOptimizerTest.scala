package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term._
import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{Type, TypeClassInstance}

class TermOptimizerTest extends LanguageSpec {
  "TermOptimizer should" - {
    "optimize application of literals" - {
      "integers" in {
        optimize(litInt(2)) shouldBe Value(2)
      }
    }

    "optimize sums of constants" - {
      "2 + 3" in {
        val term = Apply(Apply(Apply(Id("add"), Inst(addDouble)), litInt(2)), litInt(3))
        optimize(term) shouldBe Value(5)
      }

      "2 + 3 + 4 + 5" in {
        val term = add(addDouble, add(addDouble, add(addDouble, litInt(2), litInt(3)), litInt(4)), litInt(5))
        optimize(term) shouldBe Value(14)
      }
    }

    "replace optimized terms in let bindings" - {
      "x = 2 in x" in {
        val term = Let("z", Lambda("pred", litInt(2)), Apply(Id("z"), Inst(addDouble)))
        optimize(term) shouldBe Value(2)
      }
    }

    "optimize children" - {
      "of lambdas" in {
        val term = Lambda("x", Apply(Lit(LitInt(2)), Inst(numDouble)))
        //val Value(f) = optimize(term)
        optimize(term) shouldBe Lambda("x", Value(2))
      }
    }

    "bug1" in {
      // This does not fail, but it will refer to a "a" in another register!
      val optimizedConstFunc = optimize(Lambda("b", Id("a")))
      val term = Let("a", Value(123), Apply(optimizedConstFunc, Id("a")))
      optimize(term) shouldBe Value(123)
    }

    "bug2" in {
      pending
      val term = optimize(Let("a", Value(123), Lambda("b", Id("a"))))
      val Value(f) = optimize(term)
      f.asInstanceOf[Any => Any](456) shouldBe 123
    }
  }

  lazy val optimizer = new TermOptimizer(new RegisterBasedInterpreter)

  def optimize(term: Term): Term = optimizer.optimize(term)
  def litInt(n: Int): Term = Apply(Lit(LitInt(n)), Inst(numDouble))
  def add(inst: TypeClassInstance, a: Term, b: Term): Term =
    Apply(Apply(Apply(Id("add"), Inst(inst)), a), b)

  lazy val numDouble: TypeClassInstance = TypingConfig.instance(Predicate("Num", List(Type.Double))).unsafeRight
  lazy val addDouble = TypingConfig.instance(Predicate("Add", List(Type.Double, Type.Double, Type.Double))).unsafeRight
}
