package evolution.primitive.algebra.constants
import evolution.generator.Generator
import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra.{Const, GenRepr, Sized, TestInterpreters}
import _root_.evolution.primitive.algebra.constants.generator.ConstantsGenerator
import _root_.evolution.primitive.algebra.constants.interpreter.{ConstantsBySize, ConstantsSizeEvaluator}
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import cats.kernel.Semigroup
import evolution.primitive.algebra.evolution.parser.ConstantsGrammar
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ConstantsBySizeSpec
    extends FreeSpec
    with Matchers
    with TestInterpreters
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  "A Sized Constants Interpreter" - {
    "should generate expression of the given size" in {
      forAll(sizes) { size =>
        size.actual shouldBe size.expected
      }
    }
  }

  val sizeEvaluator: ConstantsSizeEvaluator[Double] =
    new ConstantsSizeEvaluator[Double]

  val generator: Constants[GenRepr[Const[?, Int], ?], Unit] =
    new ConstantsGenerator[Const[?, Int]](sizeEvaluator)

  val sizedGenerator: Constants[Sized[GenRepr[Const[?, Int], ?], ?], Unit] =
    new ConstantsBySize[GenRepr[Const[?, Int], ?], Unit](generator, genOrMonoidK[Const[?, Int]])

  def defer[T](t: => Sized[GenRepr[Const[?, Int], ?], T]): Sized[GenRepr[Const[?, Int], ?], T] =
    size => deferGenRepr[Const[?, Int]].defer(t(size))

  def doubles: Sized[GenRepr[Const[?, Int], ?], Double] =
    size =>
      vars =>
        Generator.Or(
          List(
            sizedGenerator.double(())(size)(vars),
            sizedGenerator.add[Double](defer(doubles), defer(doubles))(Semigroup[Double])(size)(vars)
          )
    )

  def sizes: Gen[Size] =
    Gen
      .sized(size => doubles(size)(0).underlying.map(actualSize => Size(size, actualSize)))

  case class Size(expected: Int, actual: Int)

  object WithGrammar {
    //def grammar(self: ) = new ConstantsGrammar[]()
  }
}
