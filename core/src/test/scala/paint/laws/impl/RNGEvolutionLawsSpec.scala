package paint.laws.impl

import org.scalacheck.Gen
import paint.evolution.RNGEvolutionRepr
import paint.evolution.algebra.impl.RNGEvolutionAlgebra
import paint.laws.{EvolutionLaws, LawsBaseSpec}
import paint.random.{RNG, SimpleRNG}

class RNGEvolutionLawsSpec extends LawsBaseSpec[RNGEvolutionRepr, RNG] with EvolutionLaws[RNGEvolutionRepr, RNG] {
  override val E = new RNGEvolutionAlgebra
  override def worlds: Gen[RNG] =
    Gen.choose(Long.MinValue, Long.MaxValue).map(SimpleRNG)
}
