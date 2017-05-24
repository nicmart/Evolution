package paint.evolution

import paint.evolution.Evolution.sequence
import paint.random.{RNG, SimpleRNG}

import scala.collection.immutable.{Queue, Stream}
import scala.util.Random

/**
  * Created by Nicolò Martini on 12/05/2017.
  */
final case class Evolution[A](run: RNG => (RNG, A, Evolution[A])) {
    def flatMapNext[B](f: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
        Evolution { rng =>
            val (rng2, a, eva2) = run(rng)
            f(a, eva2).run(rng2)
        }

    def mapNext[B](f: (A, Evolution[A]) => (B, Evolution[B])): Evolution[B] =
        Evolution { rng =>
            val (rng2, a, eva2) = run(rng)
            val (b, evb) = f(a, eva2)
            (rng2, b, evb)
        }

    def map[B](f: A => B): Evolution[B] =
        mapNext((a, eva2) => (f(a), eva2.map(f)))

    def mapOnlyNext(f: Evolution[A] => Evolution[A]): Evolution[A] =
        mapNext((a, eva2) => (a, f(eva2)))

    def map2[B, C](evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
        Evolution.map2(f)(this, evb)

    def map2Next[B, C](evb: Evolution[B])(f: (A, Evolution[A], B, Evolution[B]) => Evolution[C]): Evolution[C] =
        Evolution.map2Next(this, evb)(f)

    def perturbate(perturbations: Evolution[A => A]): Evolution[A] =
        perturbations.flatMapNext((p, perts2) => map(p).mapOnlyNext(_.perturbate(perts2)))

    def next(nextEv: Evolution[A]): Evolution[A] =
        mapOnlyNext(_ => nextEv)

    def flatMap[B](f: A => Evolution[B]): Evolution[B] =
        flatMapNext((a, eva2) => f(a).mapOnlyNext(_ => eva2.flatMap(f)))

    def scan[Z](z: Z)(f: (Z, A) => Z): Evolution[Z] =
        mapNext { (a, eva2) =>
            (z, eva2.scan(f(z, a))(f))
        }

    def prepend(as: List[A]): Evolution[A] =
        Evolution.prepend(as)(this)

    def tail: Evolution[A] = Evolution { rng =>
        val (rng2, a, eva2) = run(rng)
        eva2.run(rng2)
    }

    def drop(n: Int): Evolution[A] = n match {
        case _ if n <= 0 => this
        case _ => tail.drop(n - 1)
    }

    def zip[B](evb: Evolution[B]): Evolution[(A, B)] =
        map2(evb)((_, _))

    def speedUp(skip: Int): Evolution[A] =
        mapOnlyNext(_.drop(skip).speedUp(skip))

    def slowDown(times: Int): Evolution[A] =
        Evolution.flatten(map(List.fill(times)(_)))

    def slowDown(times: Evolution[Int]): Evolution[A] =
        map2Next(times){ (a, eva2, n, times2) =>
            Evolution.prepend(List.fill(n)(a))(eva2.slowDown(times2))
        }

    def unfold(from: RNG): Stream[A] = {
        val (rng2, a, ev2) = run(from)
        a #:: ev2.unfold(rng2)
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

    def appendAfter(k: Int, after: => Evolution[A]): Evolution[A] = k match {
        case _ if k <= 0 => after
        case _ => flatMapNext { (a, eva2) =>
            a :: eva2.appendAfter(k - 1, after)
        }
    }

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

    def restartEvery(k: Int): Evolution[A] =
        appendAfter(k, this.restartEvery(k))

    def replaceEvery[B](k: Int, f: A => Evolution[B]): Evolution[B] =
        flatMapNext { (a, eva2) =>
            f(a).appendAfter(k, eva2.replaceEvery(k, f))
        }

        def ::(value: A): Evolution[A] = prepend(List(value))

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

    def parallel[B](generator: A => Evolution[B], n: Int): Evolution[List[B]] =
        grouped(n).flatMapNext { (points, _) => sequence(points.map(generator)) }
}

object Evolution {
    def pure[A](value: A): Evolution[A] =
        Evolution { (_, value , pure(value)) }

    def map2Next[A, B, C]
        (eva: Evolution[A], evb: Evolution[B])
        (f: (A, Evolution[A], B, Evolution[B]) => Evolution[C]): Evolution[C] = {
            eva.flatMapNext { (a, eva2) =>
                evb.flatMapNext { (b, evb2) =>
                    f(a, eva2, b, evb2)
                }
            }
    }

    def map2[A, B, C](f: (A, B) => C)(eva: Evolution[A], evb: Evolution[B]): Evolution[C] =
        map2Next(eva, evb)((a, eva2, b, evb2) => f(a, b) :: map2(f)(eva2, evb2))

    def traverse[A, B](as: List[A])(f: A => Evolution[B]): Evolution[List[B]] =
        as.foldRight[Evolution[List[B]]](pure(List())) { (a, evb) =>
            f(a).map2(evb)(_ :: _)
        }

    def sequence[A](evolutions: List[Evolution[A]]): Evolution[List[A]] =
        traverse(evolutions)(ev => ev)

    def prepend[A](as: List[A])(eva: => Evolution[A]): Evolution[A] = Evolution { rng =>
        as match {
            case Nil => eva.run(rng)
            case head :: tail => (rng, head, prepend(tail)(eva))
        }
    }

    def cycle[A](as: List[A]): Evolution[A] = prepend(as)(cycle(as))

    def transition[A](from: A)(f: A => A): Evolution[A] =
        prepend(List(from))(transition(f(from))(f))

    def transitionEvo[A](from: A)(evf: Evolution[A => A]): Evolution[A] =
        evf flatMapNext { (f, evf2) =>
            from :: transitionEvo(f(from))(evf2)
        }

    def flatten[A](evas: Evolution[List[A]]): Evolution[A] =
        evas.flatMapNext((as, evas2) => prepend(as)(flatten(evas2)))

    def sample[A](eva: Evolution[A], n: Int = 10, rng: Option[RNG] = None): List[A] =
        eva.unfold(rng.getOrElse(SimpleRNG(Random.nextLong()))).take(n).toList

    def debug[A](eva: Evolution[A], n: Int = 10)(rng: Option[RNG] = None): List[(RNG, A)] = n match {
        case 0 => Nil
        case _ =>
            val (rng2, a, eva2) = eva.run(rng.getOrElse(SimpleRNG(Random.nextLong())))
            (rng2, a) :: debug(eva2, n - 1)(Some(rng2))
    }
}
