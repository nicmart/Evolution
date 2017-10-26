package paint.laws.impl

import org.scalacheck.Gen
import paint.evolution.Repr
import paint.evolution.algebra.impl.RNGEvolutionAlgebra
import paint.laws.{EvolutionLaws, IsEq, LawsBaseSpec}
import paint.random.{RNG, SimpleRNG}

class RNGEvolutionLawsSpec extends LawsBaseSpec[Repr] with EvolutionLaws[Repr] {
  override val E = new RNGEvolutionAlgebra
  def rngs: Gen[RNG] =
    Gen.choose(Long.MinValue, Long.MaxValue).map(SimpleRNG)

  override def check[A](eq: IsEq[Repr[A]]): Unit = {
    forAll(rngs) { (world: RNG) =>
      eq.lhs.unfold(world).take(sampleSize) shouldBe eq.rhs.unfold(world).take(sampleSize)
    }
  }
}
