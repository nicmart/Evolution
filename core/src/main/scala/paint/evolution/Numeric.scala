package paint.evolution

import java.lang.StrictMath

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object Numeric {
    val int: Evolution[Int] = Evolution { rng =>
        val (i, rng2) = rng.nextInt
        (rng2, i, int)
    }

    val nonNegative: Evolution[Int] =
        int flatMapNext { (n, int2) =>
            n match {
                case Int.MinValue => nonNegative
                case _ if n < 0 => -n :: nonNegative
                case _ => n :: nonNegative
            }
        }

    def nonNegativeLessThan(n: Int): Evolution[Int] =
        nonNegative flatMapNext { (i, nonNeg2) =>
            val mod = i % n
            if (i + (n-1) - mod >= 0)
                i :: nonNegativeLessThan(n)
            else nonNegativeLessThan(n)
        }

    val double: Evolution[Double] =
        int.map { n =>
            (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
        }

    def ball(radius: Double): Evolution[Double] =
        double.map(d => radius * (d * 2 - 1))

    def normal: Evolution[Double] =
        ball(1)
            .compose(ball(1))((v1, v2) => (v1, v2, v1 * v1 + v2 * v2))
            .filter { case (_, _, s) => s > 0 && s < 1 }
            .map { case (v1, _, s) =>
                val multiplier = math.sqrt(-2 * math.log(s) / s)
                v1 * multiplier
            }

    def complement(ev: Evolution[Double], radius: Double): Evolution[Double] =
        ev.map { d =>
            math.signum(d) * (radius - math.abs(d))
        }

    def choice[A](as: IndexedSeq[A]): Evolution[A] =
        nonNegativeLessThan(as.length).map(as.apply)

}
