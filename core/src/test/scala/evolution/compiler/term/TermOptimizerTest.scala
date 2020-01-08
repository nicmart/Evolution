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
        optimize(term, Map("add" -> Value((inst: Any) => (x: Int) => (y: Int) => x + y))) shouldBe Value(5)
      }

      "2 + 3 + 4 + 5" in {
        val term = add(addDouble, add(addDouble, add(addDouble, litInt(2), litInt(3)), litInt(4)), litInt(5))
        optimize(term, Map("add" -> Value((inst: Any) => (x: Int) => (y: Int) => x + y))) shouldBe Value(14)
      }
    }

    "replace optimized terms in let bindings" - {
      "x = 2 in x" in {
        val term = Let("z", Lambda("pred", litInt(2)), Apply(Id("z"), Inst(addDouble)))
        optimize(term) shouldBe Value(2)
      }
    }

    "alpha conversions are performed when necessary" - {

      "" in {
        val term = Lambda("x", Lambda("x", Id("x")))

        val optimized = optimize(term)

        optimized shouldBe Lambda("x", Lambda("x'", Id("x'")))
      }

      "(b -> (a -> b -> a)(b)(1)) === (b -> b)" in {
        val term =
          Lambda(
            "b",
            Apply(Apply(Lambda("a", Lambda("b", Id("a"))), Id("b")), Value(1))
          )

        val optimized = optimize(term)

        optimized shouldBe Lambda("b", Id("b"))
      }

      "b = 1 in a = b in b = 2 in a == 1" in {
        val term =
          lets(
            "b" -> Value(1),
            "a" -> Id("b"),
            "b" -> Value(2)
          )(Id("a"))

        val optimized = optimize(term)

        optimized shouldBe Value(1)
      }
    }

    "lists are optimized even when not all elements are values" in {
      val term = Lit(LitList(List(Id("f1"))))
      val optimized = optimize(term, Map("f1" -> Lambda("z", Value(1))))

      optimized shouldBe Lit(LitList(List(Lambda("z", Value(1)))))
    }

    "optimize children" - {
      "of lambdas" in {
        val term = Lambda("x", Apply(Lit(LitInt(2)), Inst(numDouble)))
        //val Value(f) = optimize(term)
        optimize(term) shouldBe Lambda("x", Value(2))
      }
    }

    "beta reduction" in {
      val lambda = Lambda("x", Lambda("y", Lambda("z", Id("z"))))
      val appliedLambda = Apply(Apply(Apply(lambda, Value("a")), Value("b")), Value("c"))
      optimize(appliedLambda) shouldBe Value("c")
    }

    "bug1" in {
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

  def optimize(term: Term, defs: Map[String, Term] = Map.empty): Term = optimizer.optimize(term, defs)
  def litInt(n: Int): Term = Apply(Lit(LitInt(n)), Inst(numDouble))
  def add(inst: TypeClassInstance, a: Term, b: Term): Term =
    Apply(Apply(Apply(Id("add"), Inst(inst)), a), b)

  lazy val numDouble: TypeClassInstance = TypingConfig.instance(Predicate("Num", List(Type.Double))).unsafeRight
  lazy val addDouble = TypingConfig.instance(Predicate("Add", List(Type.Double, Type.Double, Type.Double))).unsafeRight
}
