package evolution.generator.instances
import cats.{Defer, MonoidK}
import evolution.generator.Generator
import evolution.primitive.algebra.GenRepr
import org.scalacheck.Gen

trait GeneratorInstances {
  def genOrMonoidK[R[_]]: MonoidK[GenRepr[R, ?]] = new MonoidK[GenRepr[R, ?]] {
    override def empty[A]: GenRepr[R, A] = _ => Generator.Fail()
    override def combineK[A](x: GenRepr[R, A], y: GenRepr[R, A]): GenRepr[R, A] = n => Generator.Or(List(x(n), y(n)))
  }

  def deferGenRepr[R[_]]: Defer[GenRepr[R, ?]] = new Defer[GenRepr[R, ?]] {
    override def defer[A](fa: => GenRepr[R, A]): GenRepr[R, A] =
      n => deferGenerator.defer(fa(n))
  }

  val deferGenerator: Defer[Generator] = new Defer[Generator] {
    override def defer[A](fa: => Generator[A]): Generator[A] = Generator.Unknown(Gen.lzy(fa.underlying))
  }
}

object GeneratorInstances extends GeneratorInstances
