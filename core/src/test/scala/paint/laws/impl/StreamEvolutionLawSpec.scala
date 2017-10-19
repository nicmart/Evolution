package paint.laws.impl

import org.scalacheck.Gen
import paint.evolution.algebra.impl.StreamEvolutionAlgebra
import paint.laws.{EvolutionLaws, LawsBaseSpec}

class StreamEvolutionLawSpec extends LawsBaseSpec[Stream, Any] with EvolutionLaws[Stream, Any] {
  override val E: StreamEvolutionAlgebra = new StreamEvolutionAlgebra
  override def worlds: Gen[Any] = Gen.const(0)
}
