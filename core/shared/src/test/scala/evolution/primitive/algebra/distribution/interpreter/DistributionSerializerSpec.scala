package evolution.primitive.algebra.distribution.interpreter
import cats.Id
import evolution.generator.instances.GeneratorInstances
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{ FreeSpec, Inspectors, Matchers }

class DistributionSerializerSpec
    extends FreeSpec
    with Matchers
    with Inspectors
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  val interpreter = new DistributionSerializer[Id]
  import interpreter._

  "A DistributionSerializer should serialize" - {
    "uniform distributions" in {
      forAll { (from: String, to: String) =>
        uniform(_ => from, _ => to)(Nil) shouldBe s"uniform($from, $to)"
      }
    }
  }
}
