package evolution.primitive.algebra.constants
import evolution.generator.Generator
import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra._
import _root_.evolution.primitive.algebra.constants.generator.ConstantsGenerator
import _root_.evolution.primitive.algebra.constants.interpreter.{
  ConstantsSerializer,
  ConstantsSizeEvaluator,
  ConstantsBySize
}
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import cats.kernel.Semigroup
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ConstantsBySizeSpec
    extends FreeSpec
    with Matchers
    with TestInterpreters
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxDiscarded = 100, minSuccessful = 100, maxSize = 200)

  "A Sized Constants Interpreter" - {
    "should generate expression of the given size" in {
      forAll(sizes) { size =>
        size.actual shouldBe size.expected
      }
    }
  }

  val serializer: ConstantsSerializer.type =
    ConstantsSerializer

  val sizeEvaluator: ConstantsSizeEvaluator.type =
    ConstantsSizeEvaluator

  val generator: Constants[GenRepr[Const[?, Int], ?], Double] =
    new ConstantsGenerator[Const[?, Int]](sizeEvaluator)

  val sizedGenerator: Constants[Sized[GenRepr[Const[?, Int], ?], ?], Double] =
    new ConstantsBySize[GenRepr[Const[?, Int], ?]](generator, genOrMonoidK[Const[?, Int]])

  val doubleGenerator: Generator[Int] =
    Generator.Unknown(Gen.const(0))

  def defer[T](t: => Sized[GenRepr[Const[?, Int], ?], T]): Sized[GenRepr[Const[?, Int], ?], T] =
    size => deferGenRepr[Const[?, Int]].defer(t(size))

  def doubles: Sized[GenRepr[Const[?, Int], ?], Double] =
    size =>
      vars =>
        Generator.Or(
          List(
            if (size <= 0) doubleGenerator else Generator.Fail(),
            sizedGenerator.add[Double](defer(doubles), defer(doubles))(Semigroup[Double])(size)(vars)
          )
    )

  def sizes: Gen[Size] =
    Gen
      .sized(size => doubles(size)(0).underlying.map(actualSize => Size(size, actualSize)))

  case class Size(expected: Int, actual: Int)
}
