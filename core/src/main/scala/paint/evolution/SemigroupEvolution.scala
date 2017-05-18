package paint.evolution

import cats.data.NonEmptyList
import cats.kernel.{Group, Semigroup}

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object SemigroupEvolution {
    def integrate[A: Semigroup](v: Evolution[A])(start: A): Evolution[A] =
        integrateMulti(v)(List(start))

    def integrateMulti[A: Semigroup](f: Evolution[A])(as: List[A]): Evolution[A] =
        as.foldRight(f){ (start, ev) =>
            ev.scan(start)((z, a) => implicitly[Semigroup[A]].combine(z, a))
        }

    def differentiate[A: Group](f: Evolution[A]): Evolution[A] =
        f.slidingPairs.map{ case (a1, a2) => implicitly[Group[A]].remove(a1, a2) }

    def translate[A: Semigroup](ev1: Evolution[A], ev2: Evolution[A]): Evolution[A] =
        ev1.compose(ev2)((a1, a2) => implicitly[Semigroup[A]].combine(a1, a2))
}
