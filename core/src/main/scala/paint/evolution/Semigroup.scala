package paint.evolution

import cats.data.NonEmptyList
import cats.kernel.Semigroup

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object Semigroup {
    def integrate[A: Semigroup](v: Evolution[A])(start: A): Evolution[A] =
        integrateMulti(v)(List(start))

    def integrateMulti[A: Semigroup](f: Evolution[A])(as: List[A]): Evolution[A] =
        as.foldRight(f){ (start, ev) =>
            ev.scan(start)((z, a) => implicitly[Semigroup[A]].combine(z, a))
        }
}
