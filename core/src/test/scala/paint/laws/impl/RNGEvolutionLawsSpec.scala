package paint.laws.impl

import org.scalacheck.Gen
import paint.evolution.{Evolution, NumericEvolutions}
import paint.evolution.`new`.impl.RNGEvolutions
import paint.laws.{EvolutionLaws, LawsBaseSpec}
import paint.random.{RNG, SimpleRNG}

class RNGEvolutionLawsSpec extends LawsBaseSpec[Evolution, RNG] with EvolutionLaws[Evolution, RNG] {
  override val E = new RNGEvolutions
  override def validWorlds: Gen[RNG] =
    Gen.choose(Long.MinValue, Long.MaxValue).map(SimpleRNG)

  import E._
  "map compose" in {
    forAll (intEvolutions, intFunctions, intFunctions) { (evo, f, g) =>
      check(covariantComposition[Int, Int, Int](evo, f, g))
    }
  }
}
