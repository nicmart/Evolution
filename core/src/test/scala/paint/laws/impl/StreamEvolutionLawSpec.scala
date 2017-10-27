package paint.laws.impl

import paint.evolution.algebra.interpreter.StreamIntepreter
import paint.laws.{EvolutionLaws, IsEq, LawsBaseSpec}

class StreamEvolutionLawSpec extends LawsBaseSpec[Stream] with EvolutionLaws[Stream] {
  override val E: StreamIntepreter = new StreamIntepreter
  override def check[A](eq: IsEq[Stream[A]]): Unit =
    assert(eq.lhs.take(100) == eq.rhs.take(100))
}
