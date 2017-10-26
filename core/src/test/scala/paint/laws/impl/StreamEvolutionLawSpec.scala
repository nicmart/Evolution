package paint.laws.impl

import paint.evolution.algebra.impl.StreamEvolutionAlgebra
import paint.laws.{EvolutionLaws, IsEq, LawsBaseSpec}

class StreamEvolutionLawSpec extends LawsBaseSpec[Stream] with EvolutionLaws[Stream] {
  override val E: StreamEvolutionAlgebra = new StreamEvolutionAlgebra
  override def check[A](eq: IsEq[Stream[A]]): Unit =
    assert(eq.lhs.take(100) == eq.rhs.take(100))
}
