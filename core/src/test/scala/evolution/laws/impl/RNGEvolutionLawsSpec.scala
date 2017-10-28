package evolution.laws.impl

import org.scalacheck.Gen
import evolution.algebra.interpreter.RNGInterpreter
import evolution.algebra.representation.RNGRepr
import evolution.laws.{EvolutionLaws, IsEq, LawsBaseSpec}
import evolution.random.RNG

class RNGEvolutionLawsSpec extends LawsBaseSpec[RNGRepr] with EvolutionLaws[RNGRepr] {
  override val E = new RNGInterpreter
  def rngs: Gen[RNG] =
    Gen.choose(Long.MinValue, Long.MaxValue).map(RNG.apply)

  override def check[A](eq: IsEq[RNGRepr[A]]): Unit = {
    forAll(rngs) { (world: RNG) =>
      eq.lhs.unfold(world).take(sampleSize) shouldBe eq.rhs.unfold(world).take(sampleSize)
    }
  }
}
