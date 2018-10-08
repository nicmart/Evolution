package evolution.primitive.algebra
import cats.MonoidK
import org.scalacheck.Gen

object generator {
  def genOrMonoidK[R[_]]: MonoidK[Composed[Gen, R, ?]] = new MonoidK[Composed[Gen, R, ?]] {
    override def empty[A]: Gen[R[A]] = Gen.fail
    override def combineK[A](x: Gen[R[A]], y: Gen[R[A]]): Gen[R[A]] = Gen.oneOf(x, y)
  }
}
