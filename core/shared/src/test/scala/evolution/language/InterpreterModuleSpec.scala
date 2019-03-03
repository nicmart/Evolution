package evolution.language
import cats.Id
import evolution.data.ExpressionModule
import org.scalatest.{ FreeSpec, Matchers }
import evolution.data.EvaluationContext._
import evolution.geometry.Point
import evolution.materialization.RNGRepr

class InterpreterModuleSpec extends FreeSpec with Matchers {
  val interpreter = new InterpreterModule with ExpressionModule[RNGRepr] {}
  import interpreter.Interpreter._, interpreter.Expr._

  "The interpreter" - {
    "should interpret Pnt" in {
      interpret(Pnt(Dbl(0), Dbl(0)))(emptyCtx) shouldBe Point(0, 0)
    }
  }
}
