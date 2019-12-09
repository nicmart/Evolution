package evolution.compiler.term

import evolution.compiler.LanguageSpec
import evolution.compiler.phases.typer.config.TypingConfig
import evolution.compiler.term.Term.Id
import evolution.compiler.types.TypeClasses.Predicate
import evolution.compiler.types.{Type, TypeClassInstance}
import evolution.geometry.Point

class ConstantsInterpreterTest extends LanguageSpec {
  "add" in {
    val add = interpret(Id("add")).asFunc3

    val doubleAdd = add(instance("Add", Type.Double, Type.Double, Type.Double))
    val pointAdd = add(instance("Add", Type.Point, Type.Point, Type.Point))

    doubleAdd(1)(2) shouldBe 3
    pointAdd(Point(1, 2))(Point(2, 3)) shouldBe Point(3, 5)
  }

  "greaterthan" in {
    val gt = interpret(Id("greaterthan")).asFunc3

    gt(instance("Comp", Type.Double))(1)(0) shouldBe true
    gt(instance("Comp", Type.Double))(1)(1) shouldBe false
    gt(instance("Comp", Type.Double))(0)(1) shouldBe false
  }

  "inrect" in {
    val const = interpret(Id("inrect")).asFunc3

    const(Point(-10, -10))(Point(10, 10))(Point.zero) shouldBe true
    const(Point(-10, -10))(Point(-5, -5))(Point.zero) shouldBe false
  }

  def interpret(term: Term): Any = (new TermInterpreter).interpret(term)

  private def instance(id: String, types: Type*): TypeClassInstance =
    TypingConfig.instance(Predicate(id, types.toList)).unsafeRight

  private implicit class CastOps(any: Any) {
    def asFunc1: Any => Any = any.asInstanceOf[Any => Any]
    def asFunc2: Any => Any => Any = any.asInstanceOf[Any => Any => Any]
    def asFunc3: Any => Any => Any => Any = any.asInstanceOf[Any => Any => Any => Any]
    def asFunc4: Any => Any => Any => Any => Any = any.asInstanceOf[Any => Any => Any => Any => Any]
  }
}
