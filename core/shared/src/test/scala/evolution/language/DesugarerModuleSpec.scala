package evolution.language
import evolution.data.ExpressionModule
import evolution.materialization.{ RNG, RNGRepr }
import evolution.data.EvaluationContext._
import cats.implicits._
import evolution.geometry.Point

class DesugarerModuleSpec extends LanguageSpec[RNGRepr] with InterpreterModule {
  import Expr._
  import Interpreter._, Desugarer._

  "Desugarer module" - {
    "should desugar" - {
      "take" in {
        toList(take(Integer(10), constant(Integer(1)))) shouldBe List.fill(10)(1)
        toList(take(Integer(10), constant(Pnt(Dbl(0), Dbl(1.1))))) shouldBe List.fill(10)(Point(0, 1.1))

        val points = integrate(Dbl(0), constant(Dbl(1)))
        toList(take(Integer(3), points)) shouldBe List(0, 1, 2)
      }

      "while" in {
        val expr = takeWhile(
          Cons(Integer(1), Cons(Integer(1), Cons(Integer(3), Empty()))),
          Lambda("x", Equals[Int](Var("x"), Integer(1)))
        )

        toList(expr) shouldBe List(1, 1)
      }
    }
  }

  def toList[T](expr: Expr[RNGRepr[T]]): List[T] = interpret(expr)(emptyCtx).iterator(RNG(0L)).take(1000).toList
}
