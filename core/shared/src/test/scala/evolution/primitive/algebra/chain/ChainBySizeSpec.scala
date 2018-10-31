package evolution.primitive.algebra.chain

import evolution.generator.instances.GeneratorInstances
import _root_.evolution.primitive.algebra.chain.generator.ChainGenerator
import _root_.evolution.primitive.algebra.chain.interpreter.{ChainBySize, ChainSerializer, ChainSizeEvaluator}
import _root_.evolution.primitive.algebra.evolution.parser._
import cats.{Defer, MonoidK, Semigroup}
import cats.implicits._
import evolution.generator.Generator
import evolution.geometry.Point
import evolution.primitive.algebra._
import org.scalacheck.Gen
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ChainBySizeSpec
    extends FreeSpec
    with Matchers
    with TestInterpreters
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxDiscarded = 100, minSuccessful = 50, maxSize = 10)

  "A Sized Chain Interpreter" - {
    "should generate double expressions of the given size" in {
      forAll(genChainOfDoubles) { actualSizeWithExpectedSize =>
        actualSizeWithExpectedSize.size shouldBe actualSizeWithExpectedSize.value
      }
    }
  }

  object TestWithSerialization {
    type S[T] = ConstString[T]
    type F[T] = ConstString[T]
    type R[T] = CtxString[T]
    val serializer: Chain[S, F, R] = new ChainSerializer
    val sizedDoubles: Sized[GenRepr[R, ?], S[Double]] = size =>
      numOfVars => if (size == 0) Generator.pure(vars => "d") else Generator.Fail()
    val sizedPoints: Sized[GenRepr[R, ?], S[Point]] = size =>
      numOfVars => if (size == 0) Generator.pure(vars => "p(x,y)") else Generator.Fail()
  }

  object TestWithSizeEvaluation {
    type S[T] = ConstString[T]
    type F[T] = ConstString[T]
    type R[T] = Int
    val interpreter: Chain[S, F, R] = new ChainSizeEvaluator[S, F]
    val sizedDoubles: Sized[GenRepr[R, ?], S[Double]] = size =>
      numOfVars => if (size == 0) Generator.pure(0) else Generator.Fail()
    val sizedPoints: Sized[GenRepr[R, ?], S[Point]] = size =>
      numOfVars => if (size == 0) Generator.pure(0) else Generator.Fail()

    def sizedFunction[T1, T2](
      t1: Sized[GenRepr[R, ?], T1],
      t2: Sized[GenRepr[R, ?], T2]
    ): Sized[GenRepr[R, ?], T1 => T2] =
      size => numOfVars => Generator.pure(size)
  }

  import TestWithSizeEvaluation._

  val generator: Chain[S, F, GenRepr[R, ?]] = new ChainGenerator(interpreter)

  val sizedGenerator: Chain[S, F, Sized[GenRepr[R, ?], ?]] =
    new ChainBySize[S, F, GenRepr[R, ?]](generator, genOrMonoidK[R])

  val orMonoidGenOfSizeBySize: MonoidK[Sized[GenRepr[R, ?], ?]] = new MonoidK[Sized[GenRepr[R, ?], ?]] {
    override def empty[A]: Sized[GenRepr[R, ?], A] =
      size => vars => Generator.Fail()
    override def combineK[A](x: Sized[GenRepr[R, ?], A], y: Sized[GenRepr[R, ?], A]): Sized[GenRepr[R, ?], A] =
      size => vars => Generator.Or(List(x(size)(vars), y(size)(vars)))
  }

  val deferGenOfSizeBySize: Defer[Sized[GenRepr[R, ?], ?]] = new Defer[Sized[GenRepr[R, ?], ?]] {
    override def defer[A](fa: => Sized[GenRepr[R, ?], A]): Sized[GenRepr[R, ?], A] =
      size => deferGenRepr[R].defer(fa(size))
  }

  def expressions(
    chainExprs: ChainExpressions[S, F, Sized[GenRepr[R, ?], ?]]
  ): EvolutionExpressions[S, F, Sized[GenRepr[R, ?], ?]] = new EvolutionExpressions[S, F, Sized[GenRepr[R, ?], ?]] {
    override def chain: ChainExpressions[S, F, Sized[GenRepr[R, ?], ?]] = chainExprs
    override def constants: ConstantsExpressions[Composed[Sized[GenRepr[R, ?], ?], S, ?]] =
      new ConstantsExpressions[Composed[Sized[GenRepr[R, ?], ?], S, ?]] {
        override def constantOf[T: Semigroup](
          constant: Composed[Sized[GenRepr[R, ?], ?], S, T]
        ): Composed[Sized[GenRepr[R, ?], ?], S, T] = constant
        override def points: Sized[GenRepr[R, ?], S[Point]] = sizedDoubles
        override def doubles: Sized[GenRepr[R, ?], S[Double]] = sizedPoints
      }
    override def binding: BindingExpressions[Sized[GenRepr[R, ?], ?]] =
      new BindingExpressions[Sized[GenRepr[R, ?], ?]] {
        override def valueOf[T](t: Sized[GenRepr[R, ?], T]): Sized[GenRepr[R, ?], T] = t
        override def function[T1, T2](
          t1: Sized[GenRepr[R, ?], T1],
          t2: Sized[GenRepr[R, ?], T2]
        ): Sized[GenRepr[R, ?], T1 => T2] = sizedFunction(t1, t2)
      }
  }

  def grammarRec(
    self: ChainExpressions[S, F, Sized[GenRepr[R, ?], ?]]
  ): ChainExpressions[S, F, Sized[GenRepr[R, ?], ?]] =
    new ChainGrammar[S, F, Sized[GenRepr[R, ?], ?]](expressions(self), sizedGenerator, orMonoidGenOfSizeBySize)

  val grammar: ChainExpressions[S, F, Sized[GenRepr[R, ?], ?]] =
    grammarRec(new ChainExpressions.Lazy[S, F, Sized[GenRepr[R, ?], ?]](grammar, deferGenOfSizeBySize))

  val sizedChainOfDoubles: Sized[GenRepr[R, ?], F[Double]] = grammar.evolutionOf[Double](sizedDoubles)

  val genChainOfDoubles = Gen.sized(s => sizedChainOfDoubles(s)(0).underlying.map(ValueWithSize(s, _)))

  case class ValueWithSize[T](size: Int, value: T)
}
