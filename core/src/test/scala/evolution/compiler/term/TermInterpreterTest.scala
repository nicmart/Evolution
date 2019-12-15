package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Term.Literal._
import evolution.compiler.term.Term._
import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.geometry.Point

import scala.util.Try

class TermInterpreterTest extends LanguageSpec {
  "literals" - {
    "integers" in {
      val term = Lit(LitInt(123))
      val result = interpreter.interpret(term).asInstanceOf[Any => Any]
      result(instance("Num", Type.Integer)) shouldBe 123
    }

    "booleans" in {
      val term = Lit(LitBool(true))
      val result = interpreter.interpret(term)
      result shouldBe true
    }

    "doubles" in {
      val term = Lit(LitDouble(1.1))
      val result = interpreter.interpret(term)
      result shouldBe 1.1
    }

    "lists" in {
      val term = Lit(LitList(List(Lit(LitBool(true)), Lit(LitDouble(1.1)))))
      val result = interpreter.interpret(term)
      result shouldBe List(true, 1.1)
    }
  }

  "identifiers" - {
    "in the register" in {
      val term = Id("x")
      val interpreter = RegisterBasedInterpreter.fresh
      val register = Map("x" -> 12345)

      interpreter.interpretRec(register)(term) shouldBe 12345
    }

    "not in the register" in {
      val term = Id("z")
      val interpreter = RegisterBasedInterpreter.fresh

      Try(interpreter.interpret(term)).isFailure shouldBe true
    }
  }

  "lambdas" - {
    "identity" in {
      val term = Lambda("x", Id("x"))

      val interpreted = interpreter.interpret(term).asInstanceOf[Any => Any]

      interpreted("anything") shouldBe "anything"
      interpreted(12345) shouldBe 12345
    }

    "with multiple vars" in {
      val term = Lambda("x", Lambda("y", Id("x")))

      val interpreted = interpreter.interpret(term).asInstanceOf[Any => Any => Any]

      interpreted("first")("second") shouldBe "first"
      interpreted(1)(2) shouldBe 1
    }
  }

  "app" - {
    "of single argument" in {
      val term = Apply(Id("f"), Lit(LitDouble(1)))
      val interpreter = RegisterBasedInterpreter.fresh
      val register = Map("f" -> ((x: Double) => x + 1))

      interpreter.interpretRec(register)(term) shouldBe 2
    }

    "of multiple arguments" in {
      val term = Apply(Apply(Id("f"), Lit(LitDouble(1))), Lit(LitDouble(2)))
      val interpreter = RegisterBasedInterpreter.fresh
      val register = Map("f" -> ((x: Double) => (y: Double) => x + y))

      interpreter.interpretRec(register)(term) shouldBe 3
    }

    "of polymorphic int literals" in {
      val term = Term.Apply(Term.Lit(LitInt(0)), Id("P0"))

      val interpreter = RegisterBasedInterpreter.fresh
      val register = Map("P0" -> instance("Num", Type.Double))

      interpreter.interpretRec(register)(term) shouldBe a[Double]
      interpreter.interpretRec(register)(term) shouldBe 0
    }

    "of custom constants" - {
      "add" in {
        val term = Apply(Id("add"), Inst(instance("Add", Type.Double, Type.Integer, Type.Double)))

        val f = interpreter.interpret(term).asInstanceOf[Any => Any => Any]
        f(3.5)(1) shouldBe 4.5
      }

      "poly add" in {
        val term = Apply(Id("add"), Id("P0"))

        val interpreter = RegisterBasedInterpreter.fresh
        val addInstance = instance("Add", Type.Double, Type.Integer, Type.Double)
        val register = RegisterBasedInterpreter.constants + ("P0" -> addInstance)

        val f = interpreter.interpretRec(register)(term).asInstanceOf[Any => Any => Any]
        f(3.5)(1) shouldBe 4.5
      }
    }
  }

  "instance" - {
    "simple" in {
      val numInstance = instance("Num", Type.Double)
      val term = Inst(numInstance)

      interpreter.interpret(term) shouldBe numInstance
    }
  }

  "let" - {
    "of a literal" in {
      val term = Let("x", Lit(LitDouble(1.1)), Id("x"))

      interpreter.interpret(term) shouldBe 1.1
    }

    "of a polymorphic expression" in {
      val term = Let("id", Lambda("x", Id("x")), Id("id"))
      val f = interpreter.interpret(term).asInstanceOf[Any => Any]
      f("abc") shouldBe "abc"
      f(1) shouldBe 1
    }

    "of a qualified polymorphic expression" in {
      val term =
        Let(
          "double",
          Lambda("additive", Lambda("x", Apply(Apply(Apply(Id("add"), Id("additive")), Id("x")), Id("x")))),
          Id("double")
        )
      val f = interpreter.interpret(term).asInstanceOf[Any => Any => Any]
      f(instance("Add", Type.Double, Type.Double, Type.Double))(1.5) shouldBe 3
      f(instance("Add", Type.Point, Type.Point, Type.Point))(Point(1, 2)) shouldBe Point(2, 4)
    }
  }

  "bug" in {
    val term = lets(
      "f" -> Lambda("x", Lambda("y", Id("x"))),
      "f1" -> Apply(Id("f"), Value(1)),
      "f2" -> Apply(Id("f"), Value(2))
    )(Apply(Id("f1"), Value(9987)))

    val value = interpreter.interpret(term)

    value shouldBe 1
  }

  private def lets(terms: (String, Term)*)(in: Term): Term =
    terms.foldRight(in) { case ((name, definition), term) => Let(name, definition, term) }

  private def instance(id: String, types: Type*): TypeClassInstance =
    TypingConfig.instance(Predicate(id, types.toList)).unsafeRight

  lazy val interpreter = new TermInterpreter
}
