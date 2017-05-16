package paint.evolution

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
object Numeric {
    val int: Evolution[Int] = Evolution { rng =>
        val (i, rng2) = rng.nextInt
        (rng2, i, int)
    }

    val double: Evolution[Double] =
        int.map { n =>
            (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
        }

    def ball(radius: Double): Evolution[Double] =
        double.map(d => radius * (d * 2 - 1))
}
