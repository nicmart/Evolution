package paint.evolution

import paint.evolution.Evolution._
import paint.random.{RNG, SimpleRNG}

import scala.collection.immutable.Stream
import scala.util.Random

final case class Evolution[+A](run: RNG => (RNG, Option[(A, Evolution[A])])) {

  // Primitives

  def flatMapNext[B](f: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
    Evolution { rng =>
      val (rng2, next) = run(rng)
      next match {
        case None => (rng2, None)
        case Some((a, eva2)) => f(a, eva2).run(rng2)
      }
    }

  def mapNext[B](f: (A, Evolution[A]) => (B, Evolution[B])): Evolution[B] = {
    //    Evolution { rng =>
    //      val (rng2, next) = run(rng)
    //      (rng2, next.map(f.tupled))
    //    }
    // Not primitive!
    flatMapNext { (a, eva) =>
      val (b, evb) = f(a, eva)
      pure(b).append(evb)
    }
  }


  def append[B >: A](other: => Evolution[B]): Evolution[B] =
    Evolution { rng =>
      val (rng2, next) = run(rng)
      next match {
        case None => other.run(rng2)
        case Some((a, eva2)) => (rng2, Some(a, eva2.append(other)))
      }
    }

  def tail: Evolution[A] =
    Evolution { rng =>
      val (rng2, opt) = run(rng)
      opt match {
        case None => (rng, None)
        case Some((a, ev2)) => ev2.run(rng2)
      }
    }

  // Non-primitives
  def flatMap[B](f: A => Evolution[B]): Evolution[B] =
    flatMapNext((a, eva2) => f(a).append(eva2.flatMap(f)))

  def map[B](f: A => B): Evolution[B] =
    mapNext((a, eva2) => (f(a), eva2.map(f)))

  def zipWith[B, C](evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
    Evolution.zipWith(f)(this, evb)

  def zipNext[B, C](evb: Evolution[B])(f: (A, Evolution[A], B, Evolution[B]) => Evolution[C]): Evolution[C] =
    Evolution.zipNext(this, evb)(f)

  def scan[Z](z: Z)(f: (Z, A) => Z): Evolution[Z] =
    mapNext { (a, eva2) =>
      (z, eva2.scan(f(z, a))(f))
    }

  def prepend[B >: A](as: List[B]): Evolution[B] =
    Evolution.prepend(as)(this)

  def first: Evolution[A] =
    flatMapNext { (a, _) => pure(a) }

  def drop(n: Int): Evolution[A] = n match {
    case _ if n <= 0 => this
    case _ => tail.drop(n - 1)
  }

  def take(n: Int): Evolution[A] = n match {
    case _ if n <= 0 => empty
    case _ => flatMapNext { (a, eva2) =>
      a :: eva2.take(n - 1)
    }
  }

  def zip[B](evb: Evolution[B]): Evolution[(A, B)] =
    zipWith(evb)((_, _))

  def slowDown(times: Int): Evolution[A] =
    flatMap { a => list(List.fill(times)(a)) }

  def slowDown(times: Evolution[Int]): Evolution[A] =
    zipNext(times) { (a, eva2, n, times2) =>
      list(List.fill(n)(a)).append(eva2.slowDown(times2))
    }

  def unfold(from: RNG): Stream[A] = {
    val (rng2, next) = run(from)
    next match {
      case None => Stream.empty
      case Some((a, eva2)) => a #:: eva2.unfold(rng2)
    }
  }

  def filter(predicate: A => Boolean): Evolution[A] =
    flatMapNext { (a, eva2) =>
      if (predicate(a)) eva2.filter(predicate).prepend(List(a))
      else eva2.filter(predicate)
    }

  def ::[B >: A](value: B): Evolution[B] =
    pure(value).append(this)

  def flattenList[B](implicit ev: A <:< List[B]): Evolution[B] =
    flatMap(a => list(a))

  def slidingPairs: Evolution[(A, A)] =
    flatMapNext { (a1, eva2) =>
      eva2.flatMapNext { (a2, eva3) =>
        (a1, a2) :: (a2 :: eva3).slidingPairs
      }
    }

  def grouped(i: Int, from: Int = 0): Evolution[List[A]] = i match {
    case _ if i <= 0 => this.map(List(_))
    case _ if from >= i => List() :: grouped(i)
    case _ => flatMapNext { (a1, eva2) =>
      eva2.grouped(i, from + 1).flatMapNext { (as, evas2) =>
        (a1 :: as) :: evas2
      }
    }
  }

  def infinite: Evolution[A] =
    append(infinite)

  def parallel[B](generator: A => Evolution[B], n: Int): Evolution[List[B]] =
    grouped(n).flatMapNext { (points, _) => sequenceParallel(points.map(generator)) }
}

object Evolution {

  // Primitives

  def empty[A]: Evolution[A] = Evolution {
    (_, None)
  }

  def pure[A](value: A): Evolution[A] =
    Evolution {
      (_, Some(value, empty[A]))
    }

  // Non-primitives
  def list[A](as: List[A]): Evolution[A] =
    as match {
      case Nil => empty
      case a :: tail => pure(a).append(list(tail))
    }

  def prepend[A](as: List[A])(eva: => Evolution[A]): Evolution[A] =
    list(as).append(eva)

  def constant[A](value: A): Evolution[A] = pure(value).infinite

  def zipNext[A, B, C]
  (eva: Evolution[A], evb: Evolution[B])
    (f: (A, Evolution[A], B, Evolution[B]) => Evolution[C]): Evolution[C] = {
    eva.flatMapNext { (a, eva2) =>
      evb.flatMapNext { (b, evb2) =>
        f(a, eva2, b, evb2)
      }
    }
  }

  def zipWith[A, B, C](f: (A, B) => C)(eva: Evolution[A], evb: Evolution[B]): Evolution[C] =
    zipNext(eva, evb)((a, eva2, b, evb2) => f(a, b) :: zipWith(f)(eva2, evb2))

  def traverse[A, B](as: List[A])(f: A => Evolution[B]): Evolution[List[B]] =
    as.foldRight[Evolution[List[B]]](pure(List())) { (a, evb) =>
      f(a).flatMap(aa => evb.map(b => aa :: b))
    }

  def traverseParallel[A, B](as: List[A])(f: A => Evolution[B]): Evolution[List[B]] =
    as.foldRight[Evolution[List[B]]](constant(List())) { (a, evb) =>
      f(a).zipWith(evb)(_ :: _)
    }

  def sequence[A](evolutions: List[Evolution[A]]): Evolution[List[A]] =
    traverse(evolutions)(ev => ev)

  def sequenceParallel[A](evolutions: List[Evolution[A]]): Evolution[List[A]] =
    traverseParallel(evolutions)(ev => ev)

  def cycle[A](as: List[A]): Evolution[A] = list(as).infinite

  def sample[A](eva: Evolution[A], n: Int = 10, rng: Option[RNG] = None): List[A] =
    eva.unfold(rng.getOrElse(SimpleRNG(Random.nextLong()))).take(n).toList

  def debug[A](eva: Evolution[A], n: Int = 10)(rng: Option[RNG] = None): List[(RNG, A)] = n match {
    case 0 => Nil
    case _ =>
      val (rng2, next) = eva.run(rng.getOrElse(SimpleRNG(Random.nextLong())))
      next match {
        case None => Nil
        case Some((a, eva2)) => (rng2, a) :: debug(eva2, n - 1)(Some(rng2))
      }
  }
}
