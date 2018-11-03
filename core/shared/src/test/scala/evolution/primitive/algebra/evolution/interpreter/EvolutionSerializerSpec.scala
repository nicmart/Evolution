package evolution.primitive.algebra.evolution.interpreter

import evolution.primitive.algebra.evolution.{Evolution, parser}
import evolution.primitive.algebra.evolution.generator.EvolutionGenerator
import evolution.primitive.algebra.evolution.parser.EvolutionExpressions.Lazy
import evolution.primitive.algebra.evolution.parser._
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import cats.kernel.Semigroup
import evolution.geometry.Point
import _root_.evolution.primitive.algebra.constants.Constants
import _root_.evolution.primitive.algebra.constants.generator.ConstantsGenerator
import _root_.evolution.generator.instances.GeneratorInstances._
import evolution.generator.Generator
import evolution.primitive.algebra.{Composed, ConstString, CtxString, GenRepr}

class EvolutionSerializerSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxDiscarded = 100, minSuccessful = 100, maxSize = 100)

  "An algebra generator" - {
    "should generate a lot of stuff" in {

      forAll(POF.gen) { list =>
        println(list)
      }

      // This goes in SO
      //doubleEvolutionGen.sample.get(Nil) shouldBe 1
//      forAll(doubleEvolutionGen) { evo =>
//        println(evo(Nil))
//      }

//      forAll(pointsGen) { c =>
//        println(c(Nil))
//        1 shouldBe 1
//      }
    }
  }

  object POF {
    def genEmpty: Gen[List[Int]] = Gen.const(Nil)
    def genCons(genHead: Gen[Int], genTail: Gen[List[Int]]): Gen[List[Int]] =
      for {
        head <- genHead
        tail <- genTail
      } yield head :: tail

    def genInt: Gen[Int] = Gen.sized(Gen.const)

    def gen: Gen[List[Int]] =
      Gen.sized(s => Gen.frequency(1 -> genEmpty, s - 1 -> Gen.resize(s - 1, genCons(genInt, Gen.lzy(gen)))))
  }

  lazy val pointsGen = constantsGen.constantOf[Point](constantsGen.points)(Semigroup[Point])(0).underlying
  lazy val doubleEvolutionGen = gen.chain.evolutionOf[Double](gen.constants.doubles)(Semigroup[Double])(0).underlying

  type F[T] = ConstString[T]
  type R[T] = CtxString[T]

  lazy val interpreter: Evolution[F, R, Double, String, String] = new EvolutionSerializer
  lazy val gen: EvolutionExpressions[F, GenRepr[R, ?]] = grammar(interpreter)
  lazy val constantsGen: ConstantsExpressions[GenRepr[R, ?]] = constantGrammar(interpreter.constants)

  def constantGrammar[R[_]](alg: Constants[R, Double]): ConstantsExpressions[GenRepr[R, ?]] =
    constantGrammarRec[R](
      alg,
      new parser.ConstantsExpressions.Lazy[GenRepr[R, ?]](constantGrammar(alg), deferGenRepr[R])
    )

  private def constantGrammarRec[R[_]](
    alg: Constants[R, Double],
    self: ConstantsExpressions[GenRepr[R, ?]]
  ): ConstantsExpressions[GenRepr[R, ?]] = {
    val constantsGenerator = new ConstantsGenerator[R](alg)
    new ConstantsGrammar[GenRepr[R, ?]](self, constantsGenerator, genOrMonoidK[R])
  }

  def grammar[F[_], R[_]](alg: Evolution[F, R, Double, String, String]): EvolutionExpressions[F, GenRepr[R, ?]] = {
    parserGrammarRec[F, R](alg, new Lazy[F, GenRepr[R, ?]](grammar(alg), deferGenRepr[R]))
  }

  private def parserGrammarRec[F[_], R[_]](
    alg: Evolution[F, R, Double, String, String],
    self: EvolutionExpressions[F, GenRepr[R, ?]]
  ): EvolutionExpressions[F, GenRepr[R, ?]] = {
    val evolutionGenerator = new EvolutionGenerator[F, R, String](alg)
    new EvolutionGrammar[F, GenRepr[R, ?], Generator[String]](
      self,
      evolutionGenerator,
      Generator.Unknown(Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString)),
      genOrMonoidK[R]
    )
  }
}
