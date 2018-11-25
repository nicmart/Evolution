package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.Const
import evolution.primitive.algebra.constants.Constants
import evolution.typeclass.VectorSpace

class ConstantsSizeEvaluator[D] extends Constants[Const[?, Int], D] {
  override def double(d: D): Int = 0
  override def point(x: Int, y: Int): Int = 1 + x + y
  override def add[T: VectorSpace](a: Int, b: Int): Int = 1 + a + b
  override def sin(d: Int): Int = 1 + d
  override def cos(d: Int): Int = 1 + d
}
