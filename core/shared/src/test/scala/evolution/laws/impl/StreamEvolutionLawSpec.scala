package evolution.laws.impl

import evolution.algebra.interpreter.StreamInterpreter
import evolution.laws.{EvolutionLaws, IsEq, LawsBaseSpec}

class StreamEvolutionLawSpec extends LawsBaseSpec[Stream] with EvolutionLaws[Stream] {
  override val E: StreamInterpreter = new StreamInterpreter
  override def check[A](eq: IsEq[Stream[A]]): Unit =
    assert(eq.lhs.take(100) == eq.rhs.take(100))
}
