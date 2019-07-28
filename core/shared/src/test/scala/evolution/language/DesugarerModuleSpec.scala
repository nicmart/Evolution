package evolution.language
import cats.implicits._
import evolution.data.EvaluationContext._
import evolution.geometry.Point
import evolution.materialization.Iterable

// TODO This is testing the desugaring through the interpreter.
class DesugarerModuleSpec extends LanguageSpec[Iterable] with IterableInterpreterModule {
  import Desugarer._
  import Expr._
  import Interpreter._

  "Desugarer module" - {
    "should desugar" - {

      "while" in {
        val expr = TakeWhile(
          Cons(Integer(1), Cons(Integer(1), Cons(Integer(3), Empty()))),
          Lambda("x", Equals[Int](Var("x"), Integer(1)))
        )

        toList(expr) shouldBe List(1, 1)
      }

      "zipWith" in {
        val expr = ZipWith(
          Expr.Constant(Dbl(0)),
          Expr.Constant(Dbl(0)),
          Lambda[Double, Double => Point]("x", Lambda[Double, Point]("y", Pnt(Var("x"), Var("y"))))
        )
        toList(expr).take(10) shouldBe List.fill(10)(Point(0, 0))
      }

      "withFirst" in {
        val expr = withFirst[Double, Double](
          Cons(Dbl(1), Cons(Dbl(2), Empty())),
          Lambda[Double, Iterable[Double]]("x", Expr.Constant(Var("x")))
        )
        toList(expr).take(2) shouldBe List(1, 1)
      }

      "withFirst2" in {
        val expr = withFirst2[Double, Double](
          Cons(Dbl(1), Cons(Dbl(2), Empty())),
          lambda2[Double, Double, Iterable[Double]]("x", "y", Expr.Constant(Var("y")))
        )
        toList(expr).take(2) shouldBe List(2, 2)
      }

      "withFirst3" in {
        val expr = withFirst3[Double, Double](
          Cons(Dbl(1), Cons(Dbl(2), Cons(Dbl(3), Empty()))),
          lambda3[Double, Double, Double, Iterable[Double]]("x", "y", "z", Expr.Constant(Var("z")))
        )
        toList(expr).take(2) shouldBe List(3, 3)
      }
    }
  }

  def toValue[T](expr: Expr[T]): T = interpret(expr)(emptyCtx)
  def toList[T](expr: Expr[Iterable[T]]): List[T] = interpret(expr)(emptyCtx).run.take(1000).toList
}
