package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Const
import evolution.primitive.algebra.constants.Constants

object ConstantsSizeEvaluator extends Constants[Const[?, Int], Double] {
  override def double(d: Double): Int = 0
  override def point(x: Int, y: Int): Int = 1 + x + y
  override def add[T: Semigroup](a: Int, b: Int): Int = 1 + a + b
}
