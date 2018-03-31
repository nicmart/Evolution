package evolution.algebra.representation

final case class SeedRepr[@specialized(Int, Double) +A](
  run: Long => (Long, Option[(A, SeedRepr[A])])
) extends StateRepr[A, Long, SeedRepr[A]]
