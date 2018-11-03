package evolution.primitive.algebra.chain.interpreter
import evolution.primitive.algebra.Const
import evolution.primitive.algebra.chain.Chain

class ChainSizeEvaluator[F[_]] extends Chain[F, Const[?, Int]] {
  override def empty[A]: Int = 0
  override def cons[A](head: Int, tail: Int): Int = 1 + head + tail
  override def mapEmpty[A](eva: Int, eva2: Int): Int = 1 + eva + eva2
  override def mapCons[A, B](eva: Int)(f: Int): Int = 1 + eva + f
}
