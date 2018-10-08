package evolution.primitive.algebra
import cats.Defer
import org.scalacheck.Gen

object defer {
  def genDefer[R[_]]: Defer[Composed[Gen, R, ?]] = new Defer[Composed[Gen, R, ?]] {
    override def defer[A](fa: => Gen[R[A]]): Gen[R[A]] = Gen.lzy(fa)
  }
}
