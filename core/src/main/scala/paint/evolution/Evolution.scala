package paint.evolution

import paint.evolution.Evolution._
import paint.random.{RNG, SimpleRNG}

import scala.collection.immutable.{Queue, Stream}
import scala.util.Random

final case class Evolution[A](run: RNG => (RNG, Option[(A, Evolution[A])])) {

    // Primitives

    def flatMapNext[B](f: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
        Evolution { rng =>
            val (rng2, next) = run(rng)
            next match {
                case None => (rng2, None)
                case Some((a, eva2)) => f(a, eva2).run(rng2)
            }
        }

    def mapNext[B](f: (A, Evolution[A]) => (B, Evolution[B])): Evolution[B] =
        Evolution { rng =>
            val (rng2, next) = run(rng)
            (rng2, next.map(f.tupled))
        }

    def append(other: => Evolution[A]): Evolution[A] =
        Evolution { rng =>
            val (rng2, next) = run(rng)
            next match {
                case None => other.run(rng2)
                case Some((a, eva2)) => (rng2, Some(a, eva2.append(other)))
            }
        }

    // Non-primitives
    def flatMap[B](f: A => Evolution[B]): Evolution[B] =
        flatMapNext((a, eva2) => f(a).append(eva2.flatMap(f)))

    def flatten[B](implicit ev: A =:= Evolution[B]): Evolution[B] =
        flatMap(identity[A])

    def map[B](f: A => B): Evolution[B] =
        mapNext((a, eva2) => (f(a), eva2.map(f)))

    def mapOnlyNext(f: Evolution[A] => Evolution[A]): Evolution[A] =
        mapNext((a, eva2) => (a, f(eva2)))

    def map2[B, C](evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
        Evolution.map2(f)(this, evb)

    def zipWith[B, C](evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
        Evolution.zipWith(f)(this, evb)

    def zipNext[B, C](evb: Evolution[B])(f: (A, Evolution[A], B, Evolution[B]) => Evolution[C]): Evolution[C] =
        Evolution.zipNext(this, evb)(f)

    def scan[Z](z: Z)(f: (Z, A) => Z): Evolution[Z] =
        mapNext { (a, eva2) =>
            (z, eva2.scan(f(z, a))(f))
        }

    def prepend(as: List[A]): Evolution[A] =
        Evolution.prepend(as)(this)

    def first: Evolution[A] =
        flatMapNext { (a, _) => pure(a) }

    def tail: Evolution[A] =
        flatMapNext { (_, eva2) =>
            eva2
        }

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

    def speedUp(skip: Int): Evolution[A] =
        mapOnlyNext(_.drop(skip).speedUp(skip))

    def slowDown(times: Int): Evolution[A] =
        flatMap { a => list(List.fill(times)(a)) }

    def slowDown(times: Evolution[Int]): Evolution[A] =
        zipNext(times){ (a, eva2, n, times2) =>
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

    def dropWhile(predicate: A => Boolean): Evolution[A] =
        flatMapNext { (a, next) =>
            if (predicate(a)) next.dropWhile(predicate)
            else a :: next
        }

    def appendAfter(k: Int, after: => Evolution[A]): Evolution[A] =
        take(k).append(after)

    def flatMapNextAfter(k: Int, f: (A, Evolution[A]) => Evolution[A]): Evolution[A] =
        flatMapNext { (a, eva2) => k match {
            case _ if k <= 0 => f(a, eva2)
            case _ => flatMapNext { (a, eva2) =>
                a :: eva2.flatMapNextAfter(k - 1, f)
            }
        }}

    def flatMapNextEvery(k: Int, f: (A, Evolution[A]) => Evolution[A]): Evolution[A] =
        flatMapNext { (a, eva2) =>
            flatMapNextAfter(k, (a, eva3) => f(a, eva3).flatMapNextEvery(k, f))
        }

    def ::(value: A): Evolution[A] =
        pure(value).append(this)

    def flattenList[B](implicit ev: A <:< List[B]): Evolution[B] =
        flatMap(a => list(a))

    def slidingPairs: Evolution[(A, A)] =
        flatMapNext { (a1, eva2) =>
            eva2.flatMapNext { (a2, _) =>
                (a1, a2) :: (a2 :: eva2).slidingPairs
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

    def empty[A]: Evolution[A] = Evolution { (_, None) }

    def pure[A](value: A): Evolution[A] =
        Evolution { (_, Some(value , empty[A])) }

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

    def map2[A, B, C](f: (A, B) => C)(eva: Evolution[A], evb: Evolution[B]): Evolution[C] =
        eva.flatMap(a => evb.map(b => f(a, b)))

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
