package evolution.primitive.algebra.constants
import evolution.generator.Generator
import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra.{CtxString, GenRepr, Sized, TestInterpreters}
import evolution.primitive.algebra.constants.generator.ConstantsAlgebraGenerator
import evolution.primitive.algebra.constants.interpreter.{ConstantsAlgebraSerializer, SizedConstantsAlgebraInterpreter}
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import cats.kernel.Semigroup
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class SizedConstantsAlgebraInterpreterSpec
    extends FreeSpec
    with Matchers
    with TestInterpreters
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxDiscarded = 100, minSuccessful = 100, maxSize = 50)

  "A Sized Constants Interpreter" - {
    "should generate expression of the given size" in {
      forAll(genWithSize) { serialized =>
        println(serialized)
      }
    }
  }

  val serializer: ConstantsAlgebraSerializer.type =
    ConstantsAlgebraSerializer

  val generator: ConstantsAlgebra[GenRepr[CtxString, ?]] =
    new ConstantsAlgebraGenerator[CtxString](serializer)

  val sizedGenerator: ConstantsAlgebra[Sized[GenRepr[CtxString, ?], ?]] =
    new SizedConstantsAlgebraInterpreter[GenRepr[CtxString, ?]](generator, genOrMonoidK[CtxString])

  val doubleGenerator: Generator[CtxString[Double]] =
    Generator.Unknown(Gen.sized(s => Gen.const(_ => s.toString)))

  def defer[T](t: => Sized[GenRepr[CtxString, ?], T]): Sized[GenRepr[CtxString, ?], T] =
    size => deferGenRepr[CtxString].defer(t(size))

  def doubles: Sized[GenRepr[CtxString, ?], Double] =
    size =>
      vars =>
        Generator.Or(
          List(
            if (size <= 1) doubleGenerator else Generator.Fail(),
            sizedGenerator.add[Double](defer(doubles), defer(doubles))(Semigroup[Double])(size)(vars)
          )
    )

  def gen: Gen[String] =
    Gen
      .sized(size => doubles(size)(0).underlying)
      .map(ctxString => ctxString(Nil))

  def genWithSize: Gen[String] = Gen.sized(s => gen.map(out => s"size: $s => $out"))
}
