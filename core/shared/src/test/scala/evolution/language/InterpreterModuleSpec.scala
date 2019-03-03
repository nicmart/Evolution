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
  }
}
