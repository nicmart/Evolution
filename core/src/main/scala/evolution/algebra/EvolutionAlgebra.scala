package evolution.algebra

import scala.collection.immutable.Queue

trait EvolutionCoreAlgebra[Evo[+ _]] {
  val empty: Evo[Nothing]
  def cons[A](head: A, tail: => Evo[A]): Evo[A]
  def mapEmpty[A](eva: Evo[A])(eva2: => Evo[A]): Evo[A]
  def mapCons[A, B](eva: Evo[A])(f: (A, Evo[A]) => Evo[B]): Evo[B]
}

trait EvolutionAlgebra[Evo[+ _]] extends EvolutionCoreAlgebra[Evo] {
  def pure[A](a: A): Evo[A] =
    cons(a, empty)

  def constant[A](a: A): Evo[A] =
    cons(a, constant(a))

  def concat[A](evo1: Evo[A], evo2: => Evo[A]): Evo[A] =
    mapEmpty(
      mapCons(evo1) { (a, evo12) => cons(a, concat(evo12, evo2)) }
    )(evo2)

  def seq[A](as: List[A]): Evo[A] =
    as match {
      case Nil => empty
      case head :: tail => concat(pure(head), seq(tail))
    }

  def flatMap[A, B](eva: Evo[A])(f: A => Evo[B]): Evo[B] =
    mapCons(eva) { (a, eva2) => concat(f(a), flatMap(eva2)(f)) }

  def head[A](eva: Evo[A]): Evo[A] =
    mapCons(eva) { (a, eva2) => pure(a) }

  def tail[A](eva: Evo[A]): Evo[A] =
    mapCons(eva) { (a, eva2) => eva2 }

  def take[A](evo: Evo[A], n: Int): Evo[A] = {
    if (n <= 0) empty else mapCons(evo) { (a, eva2) => concat(pure(a), take(eva2, n - 1)) }
  }

  def drop[A](evo: Evo[A], n: Int): Evo[A] = {
    if (n <= 0) evo else mapCons(evo) { (a, eva2) => drop(eva2, n - 1) }
  }

  def scan[Z, A](evo: Evo[A])(z: Z)(f: (Z, A) => Z): Evo[Z] =
    cons(z, mapCons(evo) { (a, evo2) => scan(evo2)(f(z, a))(f) })

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
    flatten(zipWith(eva, evn) { (a, n) => seq(List.fill(n)(a)) })

  def zipWith[A, B, C](eva: Evo[A], evb: Evo[B])(f: (A, B) => C): Evo[C] = {
    mapCons(eva) { (a, eva2) =>
      mapCons(evb) { (b, evb2) =>
        concat(pure(f(a, b)), zipWith(eva2, evb2)(f))
      }
    }
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

  def grouped[A](eva: Evo[A])(i: Int, from: Int = 0): Evo[List[A]] =
    i match {
      case _ if i <= 0 => cyclic(pure(List()))
      case _ if from >= i => cons(Nil, grouped(eva)(i))
      // 0 <= from < i
      case _ => mapCons(eva) { (a, eva2) =>
        mapCons(grouped(eva2)(i, from + 1)) { (as, evas) =>
          cons(a :: as, evas)
        }
      }
    }

  def sequenceParallel[A](evs: Queue[Evo[A]]): Evo[A] = {
    if (evs.isEmpty) empty
    else {
      val (first, rest) = evs.dequeue
      mapCons(first) { (a, first2) =>
        cons(a, sequenceParallel(rest.enqueue(first2)))
      }
    }
  }

  private def memoize[T](f: => T => T): T = {
    lazy val fixedpoint: T = f(fixedpoint)
    fixedpoint
  }
}
