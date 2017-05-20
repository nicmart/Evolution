package paint.evolution

import cats.data.NonEmptyList
import cats.kernel.{Group, Semigroup}
import cats.syntax.semigroup._
import Evolution._

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object SemigroupEvolution {
    def integrate[A: Semigroup](v: Evolution[A])(start: A): Evolution[A] =
        integrateMulti(v)(List(start))

    def integrateMulti[A: Semigroup](f: Evolution[A])(as: List[A]): Evolution[A] =
        as.foldRight(f){ (start, ev) =>
            ev.scan(start)((z, a) => z |+| a)
        }

    def solve[A: Semigroup](equation: Evolution[A => A])(start: A): Evolution[A] = {
        transitionEvo(start)(equation.map(eq => (x: A) => eq(x) |+| x))
    }

    def solveIntegral[A: Semigroup](
        equation: Evolution[A => A],
        predicate: (A, A) => Boolean
    )(start: A): Evolution[A] = {
        equation.flatMapNext { (f, eq2) =>
            val v = f(start)
            val next = start |+| v
            if (predicate(next, v)) {
                start :: solveIntegral(eq2, predicate)(f(start) |+| start)
            } else {
                solveIntegral(eq2, predicate)(start)
            }
        }
    }

    def solveIntegralTest[A: Semigroup](
        equation: Evolution[A => A],
        predicate: (A, A) => Boolean
    )(start: A): Evolution[A] = {
        equation.flatMapNext { (f, eq2) =>
            val v = f(start)
            val next = start |+| v
            if (predicate(next, v)) {
                start :: solveIntegral(eq2, predicate)(f(start) |+| start)
            } else {
                solveIntegral(eq2, predicate)(start)
            }
        }
    }

    def solveIntegral2[A: Semigroup](equation: Evolution[(A, A) => A])(x0: A, v0: A): Evolution[A] = {
        equation.flatMapNext { (f, eq2) =>
            val acc = f(x0, v0)
            val v1 = acc |+| v0
            val x1 = v1 |+| x0
            x0 :: solveIntegral2(eq2)(x1, v1)
        }
    }

    def integrateConditional[A: Semigroup](f: Evolution[A])(start: A)(p: A => Boolean): Evolution[A] = {
        val semigroup = implicitly[Semigroup[A]]
        val predicate: A => Boolean = x => !p(semigroup.combine(start, x))
        f.dropWhile(predicate).flatMapNext { (da, f2) =>
            start :: integrateConditional(f2)(semigroup.combine(start, da))(p)
        }
    }

    def differentiate[A: Group](f: Evolution[A]): Evolution[A] =
        f.slidingPairs.map{ case (a1, a2) => implicitly[Group[A]].remove(a1, a2) }

    def translate[A: Semigroup](ev1: Evolution[A], ev2: Evolution[A]): Evolution[A] =
        ev1.compose(ev2)(implicitly[Semigroup[A]].combine)
}
