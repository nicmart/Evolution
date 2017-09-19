package paint.evolution.algebra

trait EvolutionCoreAlgebra[Evo[_]] {
  def int: Evo[Int]
  def empty[A]: Evo[A]
  def pure[A](a: A): Evo[A]
  def flatMap[A, B](eva: Evo[A])(f: A => Evo[B]): Evo[B]
  def concat[A](evo1: Evo[A], evo2: => Evo[A]): Evo[A]
  def take[A](evo: Evo[A], n: Int): Evo[A]
  def drop[A](evo: Evo[A], n: Int): Evo[A]
  def scan[Z, A](evo: Evo[A])(z: Z)(f: (Z, A) => Z): Evo[Z]
  def zipWith[A, B, C](eva: Evo[A], evb: Evo[B])(f: (A, B) => C): Evo[C]
}

trait EvolutionMaterialization[Evo[_], W] {
  def run[A](evo: Evo[A], world: W): Stream[A]
}

trait EvolutionAlgebra[Evo[_]] extends EvolutionCoreAlgebra[Evo] {
  def seq[A](as: List[A]): Evo[A] =
    as match {
      case Nil => empty
      case head :: tail => concat(pure(head), seq(tail))
    }

  def map[A, B](eva: Evo[A])(f: A => B): Evo[B] =
    flatMap(eva)(f andThen pure)

  def flatten[A](ev: Evo[Evo[A]]): Evo[A] =
    flatMap(ev)(identity)

  def cyclic[A](eva: Evo[A]): Evo[A] =
    concat(eva, cyclic(eva))

  def repeat[A](eva: Evo[A], times: Int): Evo[A] =
    times match {
      case _ if times <= 0 => eva
      case _ => concat(eva, repeat(eva, times - 1))
    }

  def slowDown[A](eva: Evo[A], n: Int): Evo[A] =
    flatMap(eva) { a => repeat(pure(a), n) }

  def slowDownBy[A](eva: Evo[A], evn: Evo[Int]): Evo[A] =
    flatten(zipWith(eva, evn){ (a, n) => seq(List.fill(n)(a)) })
}

trait MaterializableEvolutionAlgebra[Evo[_], W]
  extends EvolutionAlgebra[Evo]
    with EvolutionMaterialization[Evo, W]
