package evolution.generator.instances
import cats.{Defer, MonoidK}
import evolution.generator.Generator
import evolution.primitive.algebra.GenRepr
import org.scalacheck.Gen

trait GeneratorInstances {
  private case class FailGenRepr[R[_], A]() extends GenRepr[R, A] {
    override def apply(numberOfVars: Int): Generator[R[A]] = Generator.Fail()
  }

  def genOrMonoidK[R[_]]: MonoidK[GenRepr[R, ?]] = new MonoidK[GenRepr[R, ?]] {
    override def empty[A]: GenRepr[R, A] = FailGenRepr[R, A]()
    override def combineK[A](x: GenRepr[R, A], y: GenRepr[R, A]): GenRepr[R, A] = (x, y) match {
      case (FailGenRepr(), _) => y
      case (_, FailGenRepr()) => x
      case _ =>
        n =>
          Generator.Or(List(x(n), y(n)))
    }
  }

  def deferGenRepr[R[_]]: Defer[GenRepr[R, ?]] = new Defer[GenRepr[R, ?]] {
    override def defer[A](fa: => GenRepr[R, A]): GenRepr[R, A] =
      n => deferGenerator.defer(fa(n))
  }

  val deferGenerator: Defer[Generator] = new Defer[Generator] {
    override def defer[A](fa: => Generator[A]): Generator[A] = Generator.Unknown(Gen.delay(fa.underlying))
  }
}

object GeneratorInstances extends GeneratorInstances
