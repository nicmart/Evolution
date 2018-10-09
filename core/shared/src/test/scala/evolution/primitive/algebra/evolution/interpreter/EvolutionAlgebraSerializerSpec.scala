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
import evolution.geometry.Point
import evolution.primitive.algebra.{Composed, ConstString, CtxString, Generator, defer, generator}
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.constants.generator.ConstantsAlgebraGenerator

class EvolutionAlgebraSerializerSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit override val generatorDrivenConfig =
    PropertyCheckConfig(maxDiscarded = 10000)

  "An algebra generator" - {
    "should generate a lot of stuff" in {
//      forAll(gen.list.evolutionOf[Double](gen.constants.doubles)) { evo =>
//        println(evo(Nil))
//      }

      forAll(constantsGen.constantOf[Point](constantsGen.points)) { c =>
        println(c(Nil))
        1 shouldBe 1
      }
    }
  }

  type S[T] = ConstString[T]
  type F[T] = ConstString[T]
  type R[T] = CtxString[T]

  lazy val interpreter: EvolutionAlgebra[S, F, R, String] = EvolutionAlgebraSerializer
  lazy val gen: EvolutionAlgebraExpressions[S, F, Generator[R, ?]] = grammar(interpreter)
  lazy val constantsGen: ConstantsAlgebraExpressions[S, Generator[R, ?]] = constantGrammar(interpreter.constants)

  def constantGrammar[S[_], R[_]](
    alg: ConstantsAlgebra[Composed[R, S, ?]]
  ): ConstantsAlgebraExpressions[S, Generator[R, ?]] =
    constantGrammarRec[S, R](
      alg,
      new parser.ConstantsAlgebraExpressions.Lazy[S, Generator[R, ?]](constantGrammar(alg), defer.genDefer[R])
    )

  private def constantGrammarRec[S[_], R[_]](
    alg: ConstantsAlgebra[Composed[R, S, ?]],
    self: ConstantsAlgebraExpressions[S, Generator[R, ?]]
  ): ConstantsAlgebraExpressions[S, Generator[R, ?]] = {
    val constantsGenerator = new ConstantsAlgebraGenerator[Composed[R, S, ?]](alg)
    new ConstantsAlgebraGrammar[S, Generator[R, ?]](
      self,
      constantsGenerator,
      arbitrary[Double].map(d => alg.double(d)),
      generator.genOrMonoidK
    )
  }

  def grammar[S[_], F[_], R[_]](
    alg: EvolutionAlgebra[S, F, R, String]
  ): EvolutionAlgebraExpressions[S, F, Generator[R, ?]] = {
    parserGrammarRec[S, F, R](alg, new Lazy[S, F, Generator[R, ?]](grammar(alg), defer.genDefer[R]))
  }

  private def parserGrammarRec[S[_], F[_], R[_]](
    alg: EvolutionAlgebra[S, F, R, String],
    self: EvolutionAlgebraExpressions[S, F, Generator[R, ?]]
  ): EvolutionAlgebraExpressions[S, F, Generator[R, ?]] = {
    val evolutionGenerator = new EvolutionAlgebraGenerator[S, F, R, String](alg)
    new EvolutionAlgebraGrammar[S, F, Generator[R, ?], Gen[String]](
      self,
      evolutionGenerator,
      arbitrary[Double].map(alg.constants.double),
      Gen.alphaLowerStr,
      generator.genOrMonoidK
    )
  }
}
