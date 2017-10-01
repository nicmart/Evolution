package paint.laws.impl

import org.scalacheck.Gen
import paint.evolution.algebra.MaterializableEvolutionAlgebra
import paint.evolution.algebra.impl.StreamEvolutionAlgebra
import paint.laws.{EvolutionLaws, LawsBaseSpec}

class StreamEvolutionLawSpec extends LawsBaseSpec[Stream, Any] with EvolutionLaws[Stream, Any] {
  override val E: MaterializableEvolutionAlgebra[Stream, Any] = new StreamEvolutionAlgebra
  override def worlds: Gen[Any] = Gen.const(0)
}
