package evolution.primitive.algebra.binding.interpreter
import evolution.primitive.algebra.Const
import evolution.primitive.algebra.binding.Binding

object BindingSizeEvaluator extends Binding[Const[?, Int], Unit, Unit] {
  override def v(name: Unit): Unit = ()
  override def var0[A]: Int = 0
  override def shift[A](expr: Int): Int = expr + 1
  override def let[A, B](variable: Unit, value: Int, expr: Int): Int = 1 + value + expr
  override def lambda[A, B](variable: Unit, expr: Int): Int = 1 + expr
  override def app[A, B](f: Int, a: Int): Int = 1 + f + a
  override def fix[A](expr: Int): Int = 1 + expr
}
