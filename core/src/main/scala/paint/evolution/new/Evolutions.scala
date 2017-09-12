package paint.evolution.`new`

trait Evolutions[Evolution[_], W] { self =>
  def run[A](evo: Evolution[A], world: W): Stream[A]
  def extractInt(world: W): Int
  def worlds: Evolution[W]

  def empty[A]: Evolution[A]
  def pure[A](a: A): Evolution[A]
  def flatMap[A, B](eva: Evolution[A])(f: A => Evolution[B]): Evolution[B]
  def concat[A](evo1: Evolution[A], evo2: => Evolution[A]): Evolution[A]
  def take[A](evo: Evolution[A], n: Int): Evolution[A]

  implicit def operations[A](eva: Evolution[A]): EvolutionOps[A] =
    EvolutionOps(eva)

  def map[A, B](eva: Evolution[A])(f: A => B): Evolution[B] =
    flatMap(eva)(f andThen pure)
  def flatten[A](ev: Evolution[Evolution[A]]): Evolution[A] =
    flatMap(ev)(identity)
  def seq[A](as: List[A]): Evolution[A] =
    as match {
      case Nil => empty
      case head :: tail => pure(head).append(seq(tail))
    }
  def cyclic[A](eva: Evolution[A]): Evolution[A] =
    concat(eva, cyclic(eva))


  //def int: Evolution[Int]

  final case class EvolutionOps[A](ev: Evolution[A]) {
    def flatMap[B](f: A => Evolution[B]): Evolution[B] =
      self.flatMap(ev)(f)
    def map[B](f: A => B): Evolution[B] =
      self.map(ev)(f)
    def append(otherEv: => Evolution[A]): Evolution[A] =
      self.concat(ev, otherEv)
    def take(n: Int): Evolution[A] =
      self.take(ev, n)
  }
}
