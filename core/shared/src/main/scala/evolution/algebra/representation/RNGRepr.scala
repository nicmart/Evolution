package evolution.algebra.representation

import evolution.random.RNG

final case class RNGRepr[+A](
  run: RNG => (RNG, Option[(A, RNGRepr[A])])
) extends StateRepr[A, RNG, RNGRepr[A]]


