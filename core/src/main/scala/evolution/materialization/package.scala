package evolution

import cats.effect.{ContextShift, IO}
import evolution.geometry.Point
import evolution.rng.PerlinNoise
import fs2.{Chunk, Pull, Stream}

import scala.util.Random

package object materialization {
  type Evolution[+T] = fs2.Stream[IO, T]

  implicit val ioContextShift: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  object Evolution {

    implicit class EvolutionOps[T](evo: Evolution[T]) {
      def sample(n: Int): List[T] = evo.take(n).compile.toList.unsafeRunSync()
    }

    private object Random extends Random

    def runWithSeed[T](seed: Long, evolution: Evolution[T]): Evolution[T] =
      Stream.eval(IO(Random.setSeed(seed))).flatMap(_ => evolution)

    val empty: Evolution[Nothing] = Stream.empty

    def apply[T](ts: T*): Evolution[T] = Stream.emits(ts)

    def fromSeq[T](ts: Seq[T]): Evolution[T] =
      Stream.emits(ts)

    def constant[T](t: T): Evolution[T] = Stream.constant(t)

    def cons[T](head: T, tail: => Evolution[T]): Evolution[T] =
      tail.cons1(head)

    def concat[T](it1: Evolution[T], it2: Evolution[T]): Evolution[T] =
      it1 ++ it2

    def filter[T](evo: Evolution[T])(p: T => Boolean): Evolution[T] =
      evo.filter(p)

    def range(start: Double, end: Double, step: Double): Evolution[Double] =
      step match {
        case 0 => constant(start)
        case _ =>
          val length = (1 + (end - start) / step).toInt
          Stream.range(0, length).map(n => start + n * step)
      }

    def grouped[T](evo: Evolution[T], n: Int): Evolution[List[T]] =
      evo.chunkN(n).map(_.toList)

    def ordered(evo: Evolution[Double], n: Int): Evolution[Double] =
      for {
        ns <- evo.chunkN(n).head
        ordered = ns.toVector.sorted(Ordering.Double.TotalOrdering)
        t <- Stream.emits(ordered)
      } yield t

    def distinct[T](evo: Evolution[T], n: Int): Evolution[T] =
      for {
        ns <- evo.chunkN(n).head
        t <- Stream.emits(ns.toVector.distinct)
      } yield t

    def uniform(from: Double, to: Double): Evolution[Double] = {
      val io = IO(Chunk.doubles(Array.fill(1000)(from + Random.nextDouble() * (to - from))))
      Stream.evalUnChunk(io).repeat
    }

    def uniformChoice[T](choices: List[T]): Evolution[T] =
      uniformFrom(choices.size, fromSeq(choices))

    // TODO optimise with chunks
    def uniformFrom[T](n: Int, ts: Evolution[T]): Evolution[T] =
      for {
        firstN <- ts.chunkN(n).head.map(_.toVector)
        io = IO(firstN(Random.nextInt(firstN.size)))
        t <- if (firstN.nonEmpty) Stream.eval(io).repeat else Stream.empty
      } yield t

    // TODO optimise with chunks
    def uniformDiscrete(from: Double, to: Double, step: Double): Evolution[Double] =
      Math.signum(step * (to - from)) match {
        case 0  => constant(from)
        case -1 => empty
        case 1 =>
          val size = (Math.abs((to - from) / step) + 1).toInt
          Stream.repeatEval(IO(from + Random.nextInt(size) * step))
      }

    // TODO optimise with chunks
    def normal(mu: Double, gamma: Double): Evolution[Double] =
      Stream.repeatEval(IO(Random.nextGaussian() * gamma + mu))

    def zipWith[A, B, C](fa: Evolution[A], fb: Evolution[B], f: A => B => C): Evolution[C] =
      fa.zipWith(fb)((a, b) => f(a)(b))

    def zipWithUncurried[A, B, C](f: (A, B) => C)(fa: Evolution[A], fb: Evolution[B]): Evolution[C] =
      fa.zipWith(fb)(f)

    def take[A](fa: Evolution[A], n: Int): Evolution[A] = fa.take(n)

    def takeWhile[A](fa: Evolution[A], predicate: A => Boolean): Evolution[A] =
      fa.takeWhile(predicate)

    def takeUntil[A](fa: Evolution[A], predicate: A => Boolean): Evolution[A] =
      fa.takeWhile(!predicate(_))

    def map[A, B](fa: Evolution[A], f: A => B): Evolution[B] =
      fa.map(f)

    def flatMap[A, B](fa: Evolution[A], f: A => Evolution[B]): Evolution[B] =
      fa.flatMap(f)

    def flatten[A, B](fa: Evolution[Evolution[A]]): Evolution[A] =
      fa.flatten

    // TODO chunk size matters here
    def parallel[A](ffa: Evolution[Evolution[A]]): Evolution[A] =
      ffa.parJoinUnbounded

    def integrate[A](start: A, speed: Evolution[A], add: (A, A) => A): Evolution[A] =
      speed.scanChunks(start) { (sum1, as) =>
        val sums = as.scanLeft(sum1)(add)
        (sums.last.get, sums)
      }

    def slidingMap[A, B](as: Evolution[A], f: A => A => B): Evolution[B] =
      as.zipWithNext.collect {
        case (a1, Some(a2)) => f(a1)(a2)
      }

    def iterate[A](f: A => A, start: A): Evolution[A] =
      Stream.iterate(start)(f)

    def iterate2[A](f: A => A => A, a0: A, a1: A): Evolution[A] =
      Stream.iterate((a0, a1)) { case (a1, a2) => (a2, f(a1)(a2)) }.map(_._1)

    def derive[A](as: Evolution[A], add: (A, A) => A, inverse: A => A): Evolution[A] =
      slidingMap[A, A](as, a1 => a2 => add(a2, inverse(a1)))

    def mapWithDerivative[A, B](as: Evolution[A], f: A => A => B, add: (A, A) => A, inverse: A => A): Evolution[B] =
      slidingMap[A, B](as, previous => current => f(previous)(add(current, inverse(previous))))

    def roll[A](f: Evolution[A => A], start: A): Evolution[A] =
      f.scan(start)((a, f) => f(a))

    def roll2[A](f: Evolution[A => A => A], a0: A, a1: A): Evolution[A] =
      f.scan((a0, a1)) { case ((a1, a2), f) => (a2, f(a1)(a2)) }.map(_._1)

    def solve1[A](speed: Evolution[A => A], start: A, add: (A, A) => A): Evolution[A] =
      roll(Evolution.map[A => A, A => A](speed, vFunc => a => add(a, vFunc(a))), start)

    def solve2[A](acc: Evolution[A => A => A], a0: A, v0: A, add: (A, A) => A): Evolution[A] =
      acc
        .scan((a0, v0)) {
          case ((a1, v1), f) =>
            val v2 = add(v1, f(a1)(v1))
            val a2 = add(a1, v2)
            (a2, v2)
        }
        .map(_._1)

    def withFirst1[A, B](ts: Evolution[A], f: A => Evolution[B]): Evolution[B] =
      ts.head.flatMap(f)

    def connect[A](first: Evolution[A], f: A => Evolution[A]): Evolution[A] =
      first.zipWithNext.flatMap {
        case (a, Some(_)) => Stream.emit(a)
        case (a, None)    => f(a)
      }

    def parametrizations[T](ts: Evolution[T], size: Int): Evolution[Double => T] = {
      ts.chunkN(size).map[Double => T] { chunk =>
        val actualSize = chunk.size
        t => chunk((smoothModule(t, actualSize)).toInt)
      }
    }

    private def positiveModule(n: Double, b: Int): Double = {
      if (n >= 0) n % b else (n % b) + b
    }

    private def smoothModule(n: Double, b: Int): Double = {
      if (n >= 0) {
        if ((n / b).toInt % 2 == 0) positiveModule(n, b) else b - positiveModule(n, b) - 1
      } else {
        if ((n / b).toInt % 2 == 1) positiveModule(n, b) else b - positiveModule(n, b) - 1
      }
    }

    def shuffle[T](ts: List[T]): Evolution[List[T]] =
      Stream.repeatEval(IO(Random.shuffle(ts)))

    private val range: List[Int] = (0 to 255).toList

    val noiseEvolution: Evolution[Point => Double] =
      map(
        shuffle(range),
        (permutation: List[Int]) => {
          val perlinNoise = new PerlinNoise(permutation.toArray)
          point => perlinNoise.noise(point.x, point.y)
        }
      )

    val octaveNoiseEvolution: Evolution[Int => Double => Point => Double] =
      map(
        shuffle(range),
        (permutation: List[Int]) => {
          val perlinNoise = new PerlinNoise(permutation.toArray)
          octaves => presistence => point => perlinNoise.octaveNoise(octaves, presistence, point.x, point.y)
        }
      )
  }
}
