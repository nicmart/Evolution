package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Term.Id
import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.geometry.Point
import evolution.materialization.Evolution
import Ordering.Double.TotalOrdering
import Evolution.EvolutionOps

class ConstantsInterpreterTest extends LanguageSpec {
  "comparisons" - {
    "greaterthan" in {
      val const = interpret(Id("greaterthan")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) shouldBe true
      const(instance("Comp", Type.Double))(1)(1) shouldBe false
      const(instance("Comp", Type.Double))(0)(1) shouldBe false
    }

    "greaterthanorequal" in {
      val const = interpret(Id("greaterthanorequal")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) shouldBe true
      const(instance("Comp", Type.Double))(1)(1) shouldBe true
      const(instance("Comp", Type.Double))(0)(1) shouldBe false
    }

    "lessthan" in {
      val const = interpret(Id("lessthan")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) shouldBe false
      const(instance("Comp", Type.Double))(1)(1) shouldBe false
      const(instance("Comp", Type.Double))(0)(1) shouldBe true
    }

    "lessthanorequal" in {
      val const = interpret(Id("lessthanorequal")).asFunc3

      const(instance("Comp", Type.Double))(1)(0) shouldBe false
      const(instance("Comp", Type.Double))(1)(1) shouldBe true
      const(instance("Comp", Type.Double))(0)(1) shouldBe true
    }

    "equality" in {
      val const = interpret(Id("eq")).asFunc3
      const(instance("Eq", Type.Double))(1.1)(1.1) shouldBe true
      const(instance("Eq", Type.Double))(1.1)(1.2) shouldBe false
    }

    "non-equality" in {
      val const = interpret(Id("neq")).asFunc3
      const(instance("Eq", Type.Double))(1.1)(1.1) shouldBe false
      const(instance("Eq", Type.Double))(1.1)(1.2) shouldBe true
    }
  }

  "boolean operators" - {
    "or" in {
      val const = interpret(Id("or")).asFunc3

      const(true)(false) shouldBe true
      const(false)(true) shouldBe true
      const(true)(true) shouldBe true
      const(false)(false) shouldBe false
    }

    "and" in {
      val const = interpret(Id("and")).asFunc3

      const(true)(false) shouldBe false
      const(false)(true) shouldBe false
      const(true)(true) shouldBe true
      const(false)(false) shouldBe false
    }

    "not" in {
      val const = interpret(Id("not")).asFunc1

      const(true) shouldBe false
      const(false) shouldBe true
    }

    "ifs" in {
      val const = interpret(Id("if")).asFunc3

      const(true)("a")("b") shouldBe "a"
      const(false)("a")("b") shouldBe "b"
    }
  }

  "evolutions" - {
    "empty" in {
      val const = interpret(Id("empty"))

      const shouldBe Evolution.empty
    }

    "concat" in {
      val const = interpret(Id("concat")).asFunc2

      val concat = const(Evolution("a"))(Evolution("b")).asEvo
      concat.run.toList shouldBe List("a", "b")
    }

    "cons" in {
      val const = interpret(Id("cons")).asFunc2
      val evo = const(1)(Evolution(2, 3)).asEvo
      evo.run.toList shouldBe List(1, 2, 3)
    }

    "const" in {
      val const = interpret(Id("const")).asFunc1
      val evo = const(1).asEvo
      evo.run.take(10).toList shouldBe List.fill(10)(1)
    }
  }

  def interpret(term: Term): Any = (new RegisterBasedInterpreter).interpret(term)

  lazy val addIntInstance = instance("Add", Type.Integer, Type.Integer, Type.Integer)
  lazy val addDoubleInstance = instance("Add", Type.Integer, Type.Integer, Type.Integer)
  lazy val invIntInstance = instance("Invertible", Type.Integer)

  private def instance(id: String, types: Type*): TypeClassInstance =
    TypingConfig.instance(Predicate(id, types.toList)).unsafeRight

  def sum1(n: Int): Int => Int = x => x + n
  def sum2(n: Int): Int => Int => Int = x => y => x + y + n

  private implicit class CastOps(any: Any) {
    def asFunc1: Any => Any = any.asInstanceOf[Any => Any]
    def asFunc2: Any => Any => Any = any.asInstanceOf[Any => Any => Any]
    def asFunc3: Any => Any => Any => Any = any.asInstanceOf[Any => Any => Any => Any]
    def asFunc4: Any => Any => Any => Any => Any = any.asInstanceOf[Any => Any => Any => Any => Any]
    def asEvo: Evolution[Any] = any.asInstanceOf[Evolution[Any]]
  }
}
