package evolution.materialization
import evolution.geometry.Point
import evolution.rng.PerlinNoise

import scala.collection.AbstractIterator
import scala.collection.mutable.ArrayBuffer
import evolution.typeclass.Semigroupoid
import evolution.typeclass.Invertible
import scala.util.Random

sealed trait Evolution[+T] {
  def run: Iterator[T]
}

object Evolution {

  private object Random extends Random

  def setSeed(long: Long): Unit = Random.setSeed(long)

  val empty: Evolution[Nothing] = new Evolution[Nothing] {
    val run: Iterator[Nothing] = Iterator.empty
  }

  def fromIterable[T](ts: Iterable[T]): Evolution[T] = new Evolution[T] {
    def run: Iterator[T] = ts.iterator
  }

  def constant[T](t: T): Evolution[T] = new Evolution[T] {
    val run: Iterator[T] = new AbstractIterator[T] {
      def hasNext = true
      def next = t
    }
  }

  def cons[T](head: T, tail: => Evolution[T]): Evolution[T] = new Evolution[T] {
    def run: Iterator[T] = Iterator.single(head) ++ tail.run
  }

  def concat[T](it1: Evolution[T], it2: Evolution[T]): Evolution[T] =
    new Evolution[T] {
      def run: Iterator[T] = it1.run ++ it2.run
    }

  def mapEmpty[T](it1: Evolution[T], it2: Evolution[T]): Evolution[T] = new Evolution[T] {
    def run: Iterator[T] = {
      val iterator1 = it1.run
      val iterator1HasNext = iterator1.hasNext
      if (iterator1HasNext) Iterator.single(iterator1.next()) ++ iterator1
      else it2.run
    }
  }

  def mapCons[A, B](it1: Evolution[A], f: A => Evolution[A] => Evolution[B]): Evolution[B] =
    new Evolution[B] {
      def run: Iterator[B] = {
        val iterator1 = it1.run
        val iterator1HasNext = iterator1.hasNext
        if (iterator1HasNext) {
          val head = iterator1.next()
          val tail = new Evolution[A] { val run = iterator1 }
          f(head)(tail).run
        } else empty.run
      }
    }

  def range(start: Double, end: Double, step: Double): Evolution[Double] =
    step match {
      case 0 => constant(start)
      case _ =>
        val length = ((end - start) / step).toInt + 1
        new Evolution[Double] {
          def run: Iterator[Double] = Iterator.tabulate(length)(n => start + n * step)
        }
    }

  def uniform(from: Double, to: Double): Evolution[Double] = new Evolution[Double] {
    def run: Iterator[Double] = Iterator.continually(from + Random.nextDouble() * (to - from))
  }

  def uniformChoice[T](choices: List[T]): Evolution[T] =
    uniformFrom(choices.size, fromIterable(choices))

  def uniformFrom[T](n: Int, ts: Evolution[T]): Evolution[T] =
    new Evolution[T] {
      def run: Iterator[T] = {
        val materializedTs = ts.run.take(n).toVector
        if (materializedTs.nonEmpty) Iterator.continually(materializedTs(Random.nextInt(materializedTs.size)))
        else Iterator.empty
      }
    }

  def uniformDiscrete(from: Double, to: Double, step: Double): Evolution[Double] =
    Math.signum(step * (to - from)) match {
      case 0  => constant(from)
      case -1 => empty
      case 1 =>
        new Evolution[Double] {
          private val size = (Math.abs((to - from) / step)).toInt + 1
          def run: Iterator[Double] = Iterator.continually(from + Random.nextInt(size) * step)
        }
    }

  def normal(mu: Double, gamma: Double): Evolution[Double] = new Evolution[Double] {
    def run: Iterator[Double] = Iterator.continually(Random.nextGaussian() * gamma + mu)
  }

  def zipWith[A, B, C](fa: Evolution[A], fb: Evolution[B], f: A => B => C): Evolution[C] =
    new Evolution[C] {
      def run: Iterator[C] = new AbstractIterator[C] {
        private val it1 = fa.run
        private val it2 = fb.run
        def hasNext = it1.hasNext && it2.hasNext
        def next = f(it1.next())(it2.next())
      }
    }

  def zipWithUncurried[A, B, C](f: (A, B) => C)(fa: Evolution[A], fb: Evolution[B]): Evolution[C] =
    new Evolution[C] {
      def run: Iterator[C] = new AbstractIterator[C] {
        val it1 = fa.run
        val it2 = fb.run
        def hasNext = it1.hasNext && it2.hasNext
        def next = f(it1.next(), it2.next())
      }
    }

  def take[A](n: Int, fa: Evolution[A]): Evolution[A] = new Evolution[A] {
    def run: Iterator[A] = fa.run.take(n)
  }

  def takeWhile[A](fa: Evolution[A], predicate: A => Boolean): Evolution[A] = new Evolution[A] {
    def run: Iterator[A] = fa.run.takeWhile(predicate)
  }

  def map[A, B](fa: Evolution[A], f: A => B): Evolution[B] = new Evolution[B] {
    def run: Iterator[B] = fa.run.map(f)
  }

  def flatMap[A, B](fa: Evolution[A], f: A => Evolution[B]): Evolution[B] = new Evolution[B] {
    def run: Iterator[B] = fa.run.flatMap(a => f(a).run)
  }

  def flatten[A, B](fa: Evolution[Evolution[A]]): Evolution[A] = new Evolution[A] {
    def run: Iterator[A] = fa.run.flatMap(_.run)
  }

  def parallel[A](ffa: Evolution[Evolution[A]]): Evolution[A] = new Evolution[A] {
    override def run: Iterator[A] =
      new AbstractIterator[A] {
        private val iteratorOfEvolutions = ffa.run
        private val iterators: ArrayBuffer[Iterator[A]] = ArrayBuffer.empty
        private var currentIndex = 0
        private var outerIterationEnded = false
        override def hasNext: Boolean = {
          if (outerIterationEnded) iterators(currentIndex).hasNext
          else {
            if (iteratorOfEvolutions.hasNext) {
              iterators.append(iteratorOfEvolutions.next().run)
              iterators.last.hasNext
            } else {
              outerIterationEnded = true
              iterators.head.hasNext
            }
          }
        }
        override def next(): A = {
          if (outerIterationEnded) {
            val a = iterators(currentIndex).next()
            currentIndex = (currentIndex + 1) % iterators.size
            a
          } else {
            iterators.last.next()
          }
        }
      }

  }

  def integrate[A](start: A, speed: Evolution[A], semigroupoid: Semigroupoid[A, A, A]): Evolution[A] =
    new Evolution[A] {
      def run: Iterator[A] = new AbstractIterator[A] {
        private val speedIterator = speed.run
        private var _hasNext = true
        private var _next = start
        def hasNext: Boolean = _hasNext
        def next(): A = {
          val current = _next
          if (speedIterator.hasNext) {
            _hasNext = true
            _next = semigroupoid.combine(current, speedIterator.next())
          } else {
            _hasNext = false
          }

          current
        }
      }
    }

  def derive[A](as: Evolution[A], sg: Semigroupoid.Semigroup[A], inv: Invertible[A]): Evolution[A] =
    new Evolution[A] {
      override def run: Iterator[A] = {
        val derivingIterator: Iterator[A] = as.run
        if (derivingIterator.hasNext) {
          new AbstractIterator[A] {
            var _current: A = derivingIterator.next()
            override def hasNext: Boolean = derivingIterator.hasNext
            override def next(): A = {
              val nextA = derivingIterator.next()
              val nextDerivative = sg.combine(nextA, inv.inverse(_current))
              _current = nextA
              nextDerivative
            }
          }
        } else Iterator.empty
      }
    }

  // TODO DRY with derivative?
  def mapWithDerivative[A, B](
    as: Evolution[A],
    f: A => A => B,
    sg: Semigroupoid.Semigroup[A],
    inv: Invertible[A]
  ): Evolution[B] =
    new Evolution[B] {
      override def run: Iterator[B] = {
        val derivingIterator: Iterator[A] = as.run
        if (derivingIterator.hasNext) {
          new AbstractIterator[B] {
            var _current: A = derivingIterator.next()
            override def hasNext: Boolean = derivingIterator.hasNext
            override def next(): B = {
              val nextA = derivingIterator.next()
              val nextB = f(_current)(sg.combine(nextA, inv.inverse(_current)))
              _current = nextA
              nextB
            }
          }
        } else Iterator.empty
      }
    }

  // TODO DRY, see integrate
  def solve1[A](speed: Evolution[A => A], start: A, semigroupoid: Semigroupoid[A, A, A]): Evolution[A] =
    new Evolution[A] {
      def run: Iterator[A] = new AbstractIterator[A] {
        private val speedIterator = speed.run
        private var _hasNext = true
        private var _next = start
        def hasNext: Boolean = _hasNext
        def next(): A = {
          val current = _next
          if (speedIterator.hasNext) {
            _hasNext = true
            _next = semigroupoid.combine(current, speedIterator.next()(current))
          } else {
            _hasNext = false
          }

          current
        }
      }
    }

  def solve2[A](acc: Evolution[A => A => A], a0: A, v0: A, semigroupoid: Semigroupoid[A, A, A]): Evolution[A] =
    new Evolution[A] {
      def run: Iterator[A] = new AbstractIterator[A] {
        private val accIterator = acc.run
        private var _hasNext = true
        private var _nextA = a0
        private var _nextV = v0
        def hasNext: Boolean = _hasNext
        def next(): A = {
          val currentA = _nextA
          val currentV = _nextV
          if (accIterator.hasNext) {
            _hasNext = true
            _nextV = semigroupoid.combine(currentV, accIterator.next()(currentA)(currentV))
            _nextA = semigroupoid.combine(currentA, _nextV)
          } else {
            _hasNext = false
          }

          currentA
        }
      }
    }

  def withFirst1[A, B](ts: Evolution[A], f: A => Evolution[B]): Evolution[B] = new Evolution[B] {
    override def run: Iterator[B] =
      ts.run.take(1).toList match {
        case a1 :: Nil => f(a1).run
        case _         => Iterator.empty
      }
  }

  def shuffle[T](ts: List[T]): Evolution[List[T]] = new Evolution[List[T]] {
    def run: Iterator[List[T]] = Iterator.continually(Random.shuffle(ts))
  }

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
