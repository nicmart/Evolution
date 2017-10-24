package paint.laws.impl

import org.scalacheck.Gen
import paint.evolution.EvolutionLegacy
import paint.evolution.algebra.impl.RNGEvolutionAlgebra
import paint.laws.{EvolutionLaws, LawsBaseSpec}
import paint.random.{RNG, SimpleRNG}

class RNGEvolutionLawsSpec extends LawsBaseSpec[EvolutionLegacy, RNG] with EvolutionLaws[EvolutionLegacy, RNG] {
  override val E = new RNGEvolutionAlgebra
  override def worlds: Gen[RNG] =
    Gen.choose(Long.MinValue, Long.MaxValue).map(SimpleRNG)
}
