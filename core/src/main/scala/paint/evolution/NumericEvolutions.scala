package paint.evolution

import java.lang.StrictMath

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object NumericEvolutions {
    val int: Evolution[Int] = Evolution { rng =>
        val (i, rng2) = rng.nextInt
        (rng2, Some(i, int))
    }

    val nonNegative: Evolution[Int] =
        int flatMapNext { (n, int2) =>
            n match {
                case Int.MinValue => nonNegative
                case _ if n < 0 => -n :: nonNegative
                case _ => n :: nonNegative
            }
        }

    val double: Evolution[Double] =
        int.map { n =>
            (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
        }

    def doubleRange(from: Double, to: Double): Evolution[Double] =
        double.map(d => (to - from) * d + from)

    def intRange(from: Int, to: Int): Evolution[Int] =
        doubleRange(from, to).map(_.toInt)

    def ball(radius: Double): Evolution[Double] =
        double.map(d => radius * (d * 2 - 1))

    def normal: Evolution[Double] =
        ball(1)
            .zipWith(ball(1))((v1, v2) => (v1, v2, v1 * v1 + v2 * v2))
            .filter { case (_, _, s) => s > 0 && s < 1 }
            .map { case (v1, _, s) =>
                val multiplier = math.sqrt(-2 * math.log(s) / s)
                v1 * multiplier
            }

    def choose[A](as: IndexedSeq[A]): Evolution[A] =
        int.map(i => as.apply(Math.abs(i) % as.length))

}
