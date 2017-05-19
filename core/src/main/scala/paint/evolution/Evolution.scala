package paint.evolution

import paint.evolution.Evolution.sequence
import paint.random.{RNG, SimpleRNG}

import scala.collection.immutable.{Queue, Stream}
import scala.util.Random

/**
  * Created by Nicolò Martini on 12/05/2017.
  */
case class Evolution[A](run: RNG => (RNG, A, Evolution[A])) {
    def flatMapNext[B](f: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
        Evolution { rng =>
            val (rng2, a, eva2) = run(rng)
            f(a, eva2).run(rng2)
        }

    def mapEv[B](f: (A, Evolution[A]) => (B, Evolution[B])): Evolution[B] =
        flatMapNext { (a, eva2) =>
            val (b, evb) = f(a, eva2)
            b :: evb
        }

    def mapEv[B](f: A => B, g: (A, Evolution[A]) => Evolution[B]): Evolution[B] =
        mapEv((a, eva) => (f(a), g(a, eva)))

    def map[B](f: A => B): Evolution[B] =
        mapEv(f, (_, eva) => eva.map(f))

    def mapNext(f: Evolution[A] => Evolution[A]): Evolution[A] =
        mapEv((a, eva2) => (a, f(eva2)))

    def compose[B, C](evb: Evolution[B])(f: (A, B) => C): Evolution[C] =
        Evolution.map2(f)(this, evb)

    def perturbate(perturbations: Evolution[A => A]): Evolution[A] =
//        Evolution { rng =>
//            val (rng2, a, eva2) = run(rng)
//            val (rng3, p, ps2) = perturbations.run(rng2)
//            (rng3, p(a), eva2.map(p).perturbate(ps2))
//        }
        perturbations.flatMapNext((p, perts2) => map(p).mapNext(_.perturbate(perts2)))

    def next(nextEv: Evolution[A]): Evolution[A] =
        mapNext(_ => nextEv)


    def flatMap[B](f: A => Evolution[B]): Evolution[B] =
        flatMapNext((a, eva2) => f(a).mapNext(_ => eva2.flatMap(f)))

    def scan[Z](z: Z)(f: (Z, A) => Z): Evolution[Z] =
        mapEv(f(z, _), (a, eva) => eva.scan(f(z, a))(f))

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
        compose(evb)((_, _))

    def speedUp(skip: Int): Evolution[A] =
        mapNext(_.drop(skip).speedUp(skip))

    def slowDown(times: Int): Evolution[A] =
        Evolution.flatten(map(List.fill(times)(_)))

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

    def map2[A, B, C](f: (A, B) => C)(eva: Evolution[A], evb: Evolution[B]): Evolution[C] =
        eva.flatMapNext { (a, eva2) =>
            evb.flatMapNext { (b, evb2) =>
                f(a, b) :: map2(f)(eva2, evb2)
            }
        }

    def traverse[A, B](as: List[A])(f: A => Evolution[B]): Evolution[List[B]] =
        as.foldRight[Evolution[List[B]]](pure(List())) { (a, evb) =>
            f(a).compose(evb)(_ :: _)
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
