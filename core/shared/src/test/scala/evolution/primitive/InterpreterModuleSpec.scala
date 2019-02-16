package evolution.primitive
import cats.Id
import evolution.algebra.representation.RNGRepr
import evolution.data.WithExpression
import org.scalatest.{ FreeSpec, Matchers }
import evolution.data.EvaluationContext._
import evolution.geometry.Point

class InterpreterModuleSpec extends FreeSpec with Matchers {
  val interpreter = new InterpreterModule with WithExpression[RNGRepr] {}
  import interpreter.Interpreter._, interpreter.expressionModule._

  "The interpreter" - {
    "should interpret Pnt" in {
      interpret(Pnt(Dbl(0), Dbl(0)))(emptyCtx) shouldBe Point(0, 0)
    }
  }
}
