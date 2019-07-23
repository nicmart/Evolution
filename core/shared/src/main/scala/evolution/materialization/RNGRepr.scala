package evolution.materialization
import scala.annotation.tailrec
import evolution.typeclass.VectorSpace
import evolution.geometry.Point
import evolution.rng.PerlinNoise

final case class RNGRepr[+A](
  run: RNG => (RNG, Option[(A, RNGRepr[A])])
) extends StateRepr[A, RNG, RNGRepr[A]] {
  RNGRepr.allocations += 1
  override def self: RNGRepr[A] = this
}

object RNGRepr {
  private var allocations: Int = 0

  def resetAllocationsCount(): Unit = allocations = 0
  def allocationsCount: Int = allocations

  def constant[T](t: T): RNGRepr[T] = {
    lazy val self: RNGRepr[T] = RNGRepr(rng => (rng, Some((t, self))))
    self
  }

  val empty: RNGRepr[Nothing] = RNGRepr { rng =>
    (rng, None)
  }

  def cons[T](head: T, tail: => RNGRepr[T]): RNGRepr[T] = RNGRepr { rng =>
    (rng, Some((head, tail)))
  }

  def mapEmpty[T](ts1: RNGRepr[T], ts2: RNGRepr[T]): RNGRepr[T] = RNGRepr { rng =>
    val (rng2, next) = ts1.run(rng)
    next match {
      case None => ts2.run(rng2)
      case _    => (rng2, next)
    }
  }

  def mapCons[A, B](as: RNGRepr[A], f: A => RNGRepr[A] => RNGRepr[B]): RNGRepr[B] =
    RNGRepr { rng =>
      val (rng2, next) = as.run(rng)
      next match {
        case None            => (rng2, None)
        case Some((a, eva2)) => f(a)(eva2).run(rng2)
      }
    }

  def uniform(from: Double, to: Double): RNGRepr[Double] = {
    lazy val self: RNGRepr[Double] = RNGRepr { rng =>
      val (d, rng2) = rng.nextDouble
      val scaled = from + d * (to - from)
      (rng2, Some((scaled, self)))
    }
    self
  }

  def uniformFrom[T](n: Int, ts: RNGRepr[T]): RNGRepr[T] = RNGRepr { rng =>
    val (rng2, selectedTs) = ts.collect(n, rng)
    uniformChoiceRepr(selectedTs).run(rng2)
  }

  def uniformDiscrete(start: Double, end: Double, step: Double): RNGRepr[Double] =
    uniformChoiceRepr((start to end by step).toList)

  // See https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform#Implementation
  def normal(mu: Double, sigma: Double): RNGRepr[Double] = {
    lazy val self: RNGRepr[Double] = RNGRepr { rng =>
      val (u1, rng2) = rng.nextDouble
      val (u2, rng3) = rng2.nextDouble

      // TODO: missing check that u1 is not the smallest positive double number
      val d = Math.sqrt(-2.0 * Math.log(u1)) * Math.cos(2 * Math.PI * u2)

      (rng3, Some((d * sigma + mu, self)))
    }

    self
  }

  def zipWith[A, B, C](fa: RNGRepr[A], fb: RNGRepr[B], f: A => B => C): RNGRepr[C] =
    RNGRepr { rng =>
      val (rng2, nextFa) = fa.run(rng)
      nextFa match {
        case None => (rng2, None)
        case Some((a1, fa2)) =>
          val (rng3, nextFb) = fb.run(rng2)
          nextFb match {
            case None            => (rng3, None)
            case Some((b1, fb2)) => (rng3, Some((f(a1)(b1), zipWith(fa2, fb2, f))))
          }
      }
    }

  def take[A](n: Int, fa: RNGRepr[A]): RNGRepr[A] =
    if (n <= 0) empty
    else
      RNGRepr { rng =>
        val (rng2, nextFa) = fa.run(rng)
        nextFa match {
          case None           => (rng2, None)
          case Some((a, fa2)) => (rng2, Some((a, take(n - 1, fa2))))
        }
      }

  def takeWhile[A](fa: RNGRepr[A], predicate: A => Boolean): RNGRepr[A] = RNGRepr { rng =>
    val (rng2, nextFa) = fa.run(rng)
    nextFa match {
      case None           => (rng2, None)
      case Some((a, fa2)) => if (predicate(a)) (rng2, Some((a, takeWhile(fa2, predicate)))) else (rng2, None)
    }
  }

  def map[A, B](fa: RNGRepr[A], f: A => B): RNGRepr[B] = RNGRepr { rng =>
    val (rng2, nextFa) = fa.run(rng)
    nextFa match {
      case None           => (rng2, None)
      case Some((a, fa2)) => (rng2, Some((f(a), map(fa2, f))))
    }
  }

  def flatMap[A, B](fa: RNGRepr[A], f: A => RNGRepr[B]): RNGRepr[B] =
    flatten(map(fa, f))

  def flatten[A, B](ffa: RNGRepr[RNGRepr[A]]): RNGRepr[A] = RNGRepr { rng =>
    val (rng2, nextFFa) = ffa.run(rng)
    nextFFa match {
      case None => (rng2, None)
      case Some((fa, ffa2)) =>
        val (rng3, nextFa) = fa.run(rng2)
        nextFa match {
          case None           => flatten(ffa2).run(rng3)
          case Some((a, fa2)) => cons(a, flatten(cons(fa2, ffa2))).run(rng3)
        }
    }
  }

  def integrate[A](start: A, speed: RNGRepr[A], vs: VectorSpace[A]): RNGRepr[A] = RNGRepr { rng =>
    val (rng2, nextSpeed) = speed.run(rng)
    val next = nextSpeed match {
      case None              => empty
      case Some((a, speed2)) => integrate(vs.add(start, a), speed2, vs)
    }

    (rng2, Some((start, next)))
  }

  private def uniformChoiceRepr[T](ts: List[T]): RNGRepr[T] =
    ts match {
      case Nil =>
        RNGRepr[T] { rng =>
          (rng, None)
        }
      case _ =>
        lazy val self: RNGRepr[T] = RNGRepr { rng =>
          val (n, rng2) = rng.nextInt
          val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
          val index = (d * ts.size).toInt
          (rng2, Some((ts(index), self)))
        }
        self
    }

  def shuffle[T](ts: List[T]): RNGRepr[List[T]] = {
    lazy val self: RNGRepr[List[T]] = RNGRepr { rng =>
      val (shuffledTs, rng2) = shuffled(rng, ts.toVector)
      (rng2, Some((shuffledTs.toList, self)))
    }
    self
  }

  private val range: List[Int] = (0 to 255).toList

  val noiseRNGRepr: RNGRepr[Point => Double] =
    RNGRepr.map(
      RNGRepr.shuffle(range),
      (permutation: List[Int]) => {
        val perlinNoise = new PerlinNoise(permutation.toArray)
        point =>
          perlinNoise.noise(point.x, point.y)
      }
    )

  val octaveNoiseRNGRepr: RNGRepr[Int => Double => Point => Double] =
    RNGRepr.map(
      RNGRepr.shuffle(range),
      (permutation: List[Int]) => {
        val perlinNoise = new PerlinNoise(permutation.toArray)
        octaves => presistence => point =>
          perlinNoise.octaveNoise(octaves, presistence, point.x, point.y)
      }
    )

  private def shuffled[T](rng: RNG, ts: Vector[T]): (Vector[T], RNG) = shuffledRec(rng, ts, Vector.empty)

  @tailrec
  private def shuffledRec[T](rng: RNG, ts: Vector[T], alreadyShuffled: Vector[T]): (Vector[T], RNG) =
    if (ts.isEmpty) (alreadyShuffled, rng)
    else {
      val (d, rng2) = rng.nextDouble
      val index = Math.floor(d * ts.size).toInt
      shuffledRec(rng2, ts.updated(index, ts(0)).tail, ts(index) +: alreadyShuffled)
    }
}
