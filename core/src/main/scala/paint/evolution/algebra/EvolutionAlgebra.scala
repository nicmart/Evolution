package paint.evolution.algebra

trait EvolutionCoreAlgebra[Evo[+_]] {
  def int: Evo[Int]
  def empty[A]: Evo[A]
  def pure[A](a: A): Evo[A]
  def flatMapNext[A, B](eva: Evo[A])(f: (A, Evo[A]) => Evo[B]): Evo[B]
  def concat[A](evo1: Evo[A], evo2: => Evo[A]): Evo[A]
}

trait EvolutionMaterialization[Evo[_], W] {
  def run[A](evo: Evo[A], world: W): Stream[A]
}

trait EvolutionAlgebra[Evo[+_]] extends EvolutionCoreAlgebra[Evo] {
  def seq[A](as: List[A]): Evo[A] =
    as match {
      case Nil => empty
      case head :: tail => concat(pure(head), seq(tail))
    }

  def flatMap[A, B](eva: Evo[A])(f: A => Evo[B]): Evo[B] =
    flatMapNext(eva) { (a, eva2) => concat(f(a), flatMap(eva2)(f)) }

  def head[A](eva: Evo[A]): Evo[A] =
    flatMapNext(eva) { (a, eva2) => pure(a) }

  def tail[A](eva: Evo[A]): Evo[A] =
    flatMapNext(eva) { (a, eva2) => eva2 }

  def take[A](evo: Evo[A], n: Int): Evo[A] = {
    if (n <= 0) empty[A] else flatMapNext(evo) { (a, eva2) => concat(pure(a), take(eva2, n - 1)) }
  }

  def drop[A](evo: Evo[A], n: Int): Evo[A] = {
    if (n <= 0) evo else flatMapNext(evo) { (a, eva2) => drop(eva2, n - 1) }
  }

  def scan[Z, A](evo: Evo[A])(z: Z)(f: (Z, A) => Z): Evo[Z] =
    concat(pure(z), flatMapNext(evo) { (a, evo2) => scan(evo2)(f(z, a))(f)})

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

  def zipWith[A, B, C](eva: Evo[A], evb: Evo[B])(f: (A, B) => C): Evo[C] = {
    ???
  }

  def zip[A, B](eva: Evo[A], evb: Evo[B]): Evo[(A, B)] =
    zipWith(eva, evb)((_, _))

  def filter[A](eva: Evo[A], predicate: A => Boolean): Evo[A] =
    flatMap(eva) { a =>
      if (predicate(a)) pure(a) else empty
    }

  def flattenList[A](eva: Evo[List[A]]): Evo[A] =
    flatMap(eva)(seq)

  def slidingPairs[A](eva: Evo[A]): Evo[(A, A)] = {
    val listEvo = scan[List[A], A](eva)(List.empty) {
      (as, a) => (a :: as).take(2)
    }
    flatMap(listEvo) {
      case List(a1, a2) => pure((a2, a1))
      case _ => empty
    }
  }
}

trait MaterializableEvolutionAlgebra[Evo[+_], W]
  extends EvolutionAlgebra[Evo]
    with EvolutionMaterialization[Evo, W]
