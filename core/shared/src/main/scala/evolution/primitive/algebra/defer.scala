package evolution.primitive.algebra
import cats.Defer
import org.scalacheck.Gen

object defer {
  def genDefer[R[_]]: Defer[Generator[R, ?]] = new Defer[Generator[R, ?]] {
    override def defer[A](fa: => Generator[R, A]): Generator[R, A] = Gen.lzy(fa)
  }
}
