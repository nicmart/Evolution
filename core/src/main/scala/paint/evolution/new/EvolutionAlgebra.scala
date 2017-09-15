package paint.evolution.`new`

trait EvolutionAlgebra[Evo[_]] {
  def int: Evo[Int]
  def empty[A]: Evo[A]
  def pure[A](a: A): Evo[A]
  def flatMap[A, B](eva: Evo[A])(f: A => Evo[B]): Evo[B]
  def concat[A](evo1: Evo[A], evo2: => Evo[A]): Evo[A]
  def take[A](evo: Evo[A], n: Int): Evo[A]
}
