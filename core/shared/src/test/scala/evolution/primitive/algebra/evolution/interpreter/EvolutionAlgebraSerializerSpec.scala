package evolution.primitive.algebra.evolution.interpreter

import evolution.primitive.algebra.evolution.{EvolutionAlgebra, parser}
import evolution.primitive.algebra.evolution.generator.EvolutionAlgebraGenerator
import evolution.primitive.algebra.evolution.parser.EvolutionAlgebraExpressions.Lazy
import evolution.primitive.algebra.evolution.parser._
import evolution.primitive.algebra.parser.PrimitiveParsers
import fastparse.noApi.Parser
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalacheck.Arbitrary.arbitrary
import org.scalatest.{FreeSpec, Matchers}
import cats.implicits._
import cats.kernel.Semigroup
import evolution.geometry.Point
import _root_.evolution.primitive.algebra.constants.Constants
import _root_.evolution.primitive.algebra.constants.generator.ConstantsGenerator
import _root_.evolution.generator.instances.GeneratorInstances._
import evolution.generator.Generator
import evolution.primitive.algebra.{Composed, ConstString, CtxString, GenRepr}

class EvolutionAlgebraSerializerSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
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
  lazy val doubleEvolutionGen = gen.list.evolutionOf[Double](gen.constants.doubles)(Semigroup[Double])(0).underlying

  type S[T] = ConstString[T]
  type F[T] = ConstString[T]
  type R[T] = CtxString[T]

  lazy val interpreter: EvolutionAlgebra[S, F, R, String] = EvolutionAlgebraSerializer
  lazy val gen: EvolutionAlgebraExpressions[S, F, GenRepr[R, ?]] = grammar(interpreter)
  lazy val constantsGen: ConstantsAlgebraExpressions[S, GenRepr[R, ?]] = constantGrammar(interpreter.constants)

  def constantGrammar[S[_], R[_]](alg: Constants[Composed[R, S, ?]]): ConstantsAlgebraExpressions[S, GenRepr[R, ?]] =
    constantGrammarRec[S, R](
      alg,
      new parser.ConstantsAlgebraExpressions.Lazy[S, GenRepr[R, ?]](constantGrammar(alg), deferGenRepr[R])
    )

  private def constantGrammarRec[S[_], R[_]](
    alg: Constants[Composed[R, S, ?]],
    self: ConstantsAlgebraExpressions[S, GenRepr[R, ?]]
  ): ConstantsAlgebraExpressions[S, GenRepr[R, ?]] = {
    val constantsGenerator = new ConstantsGenerator[Composed[R, S, ?]](alg)
    new ConstantsAlgebraGrammar[S, GenRepr[R, ?]](
      self,
      constantsGenerator,
      _ => Generator.Unknown(arbitrary[Double].map(d => alg.double(d))),
      genOrMonoidK[R]
    )
  }

  def grammar[S[_], F[_], R[_]](
    alg: EvolutionAlgebra[S, F, R, String]
  ): EvolutionAlgebraExpressions[S, F, GenRepr[R, ?]] = {
    parserGrammarRec[S, F, R](alg, new Lazy[S, F, GenRepr[R, ?]](grammar(alg), deferGenRepr[R]))
  }

  private def parserGrammarRec[S[_], F[_], R[_]](
    alg: EvolutionAlgebra[S, F, R, String],
    self: EvolutionAlgebraExpressions[S, F, GenRepr[R, ?]]
  ): EvolutionAlgebraExpressions[S, F, GenRepr[R, ?]] = {
    val evolutionGenerator = new EvolutionAlgebraGenerator[S, F, R, String](alg)
    new EvolutionAlgebraGrammar[S, F, GenRepr[R, ?], Generator[String]](
      self,
      evolutionGenerator,
      _ => Generator.Unknown(arbitrary[Double].map(alg.constants.double)),
      Generator.Unknown(Gen.nonEmptyListOf(Gen.alphaLowerChar).map(_.mkString)),
      genOrMonoidK[R]
    )
  }
}