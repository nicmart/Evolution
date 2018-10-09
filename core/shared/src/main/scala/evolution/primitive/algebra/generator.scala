package evolution.primitive.algebra
import cats.MonoidK
import org.scalacheck.Gen

object generator {
  def genOrMonoidK[R[_]]: MonoidK[Generator[R, ?]] = new MonoidK[Generator[R, ?]] {
    override def empty[A]: Generator[R, A] = n => Gen.fail
    override def combineK[A](x: Generator[R, A], y: Generator[R, A]): Generator[R, A] = n => Gen.oneOf(x(n), y(n))
  }
}
