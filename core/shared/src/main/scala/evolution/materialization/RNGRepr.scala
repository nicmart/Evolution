package evolution.materialization

final case class RNGRepr[+A](
  run: RNG => (RNG, Option[(A, RNGRepr[A])])
) extends StateRepr[A, RNG, RNGRepr[A]]
