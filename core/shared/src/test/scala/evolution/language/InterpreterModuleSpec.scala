package evolution.language
import evolution.data.EvaluationContext._
import evolution.data.ExpressionModule
import evolution.geometry.Point
import evolution.materialization.RNGRepr
import org.scalatest.{ FreeSpec, Matchers }

class InterpreterModuleSpec extends FreeSpec with Matchers {
  val interpreter = new InterpreterModule with ExpressionModule[RNGRepr] {}
  import interpreter.Expr._
  import interpreter.Interpreter._

  "The interpreter" - {
    "should interpret Pnt" in {
      interpret(Pnt(Dbl(0), Dbl(0)))(emptyCtx) shouldBe Point(0, 0)
    }

    "should interpret inRect statements" in {
      interpret(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(5), Dbl(5)))) shouldBe true
      interpret(InRect(Pnt(Dbl(0), Dbl(0)), Pnt(Dbl(10), Dbl(10)), Pnt(Dbl(20), Dbl(5)))) shouldBe false
    }
  }
}
