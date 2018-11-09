package evolution.primitive.algebra.constants
import evolution.generator.Generator
import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra.{Const, GenRepr, Sized, TestInterpreters}
import _root_.evolution.primitive.algebra.constants.generator.ConstantsGenerator
import _root_.evolution.primitive.algebra.constants.interpreter.{ConstantsBySize, ConstantsSizeEvaluator}
import cats.{Defer, MonoidK}
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import cats.kernel.Semigroup
import evolution.primitive.algebra.constants.parser.ConstantsParserSyntax
import evolution.primitive.algebra.evolution.parser.{ConstantsExpressions, ConstantsGrammar}
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ConstantsBySizeSpec
    extends FreeSpec
    with Matchers
    with TestInterpreters
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxDiscarded = 100, minSuccessful = 200, maxSize = 200)

  "A Sized Constants Interpreter" - {
    "should generate double expressions of the given size" in {
      forAll(sizesOf(doubles)) { size =>
        size.actual shouldBe size.expected
      }
    }

    "should generate points expressions of the given size" in {
      forAll(sizesOf(points)) { size =>
        size.actual shouldBe size.expected
      }
    }
  }

  type GenOfSize[T] = GenRepr[Const[?, Int], T]
  type GenOfSizeBySize[T] = Sized[GenOfSize, T]

  val sizeEvaluator: ConstantsSizeEvaluator[Double] =
    new ConstantsSizeEvaluator[Double]

  val generator: Constants[GenOfSize, Unit] =
    new ConstantsGenerator[Const[?, Int]](sizeEvaluator)

  val sizedGenerator: Constants[GenOfSizeBySize, Unit] =
    new ConstantsBySize[GenOfSize, Unit](generator, genOrMonoidK[Const[?, Int]])

  val orMonoidGenOfSizeBySize: MonoidK[GenOfSizeBySize] = new MonoidK[GenOfSizeBySize] {
    override def empty[A]: GenOfSizeBySize[A] =
      size => vars => Generator.Fail()
    override def combineK[A](x: GenOfSizeBySize[A], y: GenOfSizeBySize[A]): GenOfSizeBySize[A] =
      size => vars => Generator.Or(List(x(size)(vars), y(size)(vars)))
  }

  val deferGenOfSizeBySize: Defer[GenOfSizeBySize] = new Defer[GenOfSizeBySize] {
    override def defer[A](fa: => GenOfSizeBySize[A]): GenOfSizeBySize[A] =
      size => deferGenRepr[Const[?, Int]].defer(fa(size))
  }

  def doublesOld: GenOfSizeBySize[Double] =
    size =>
      vars =>
        Generator.Or(
          List(
            sizedGenerator.double(())(size)(vars),
            sizedGenerator.add[Double](deferGenOfSizeBySize.defer(doubles), deferGenOfSizeBySize.defer(doubles))(
              Semigroup[Double]
            )(size)(vars)
          )
    )

  def doubles: GenOfSizeBySize[Double] =
    WithGrammar.grammar.constantOf(WithGrammar.grammar.doubles)

  def points: GenOfSizeBySize[Double] =
    WithGrammar.grammar.constantOf(WithGrammar.grammar.points)

  def sizesOf[T](genOfSizeBySize: GenOfSizeBySize[T]): Gen[Size] =
    Gen
      .sized(size => genOfSizeBySize(size)(0).underlying.map(actualSize => Size(size, actualSize)))

  case class Size(expected: Int, actual: Int)

  object WithGrammar {
    val grammar: ConstantsExpressions[GenOfSizeBySize] =
      grammarRec(new ConstantsExpressions.Lazy(grammar, deferGenOfSizeBySize))

    def grammarRec(self: ConstantsExpressions[GenOfSizeBySize]) =
      new ConstantsGrammar[GenOfSizeBySize](self, sizedGenerator, orMonoidGenOfSizeBySize)
  }
}
