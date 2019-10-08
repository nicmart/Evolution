package evolution.materialization
import evolution.geometry.Point
import evolution.rng.PerlinNoise

import scala.collection.AbstractIterator
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed trait Evolution[+T] {
  def run: Iterator[T]
}

object Evolution {

  private object Random extends Random

  def runWithSeed[T](seed: Long, evolution: Evolution[T]): Iterator[T] = {
    Evolution.setSeed(seed)
    evolution.run
  }

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

  def take[A](fa: Evolution[A], n: Int): Evolution[A] = new Evolution[A] {
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

  def integrate[A](start: A, speed: Evolution[A], add: (A, A) => A): Evolution[A] =
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
            _next = add(current, speedIterator.next())
          } else {
            _hasNext = false
          }

          current
        }
      }
    }

  def slidingMap[A, B](as: Evolution[A], f: A => A => B): Evolution[B] =
    new Evolution[B] {
      override def run: Iterator[B] = {
        val derivingIterator: Iterator[A] = as.run
        if (derivingIterator.hasNext) {
          new AbstractIterator[B] {
            var previous: A = derivingIterator.next()
            override def hasNext: Boolean = derivingIterator.hasNext
            override def next(): B = {
              val current = derivingIterator.next()
              val b = f(previous)(current)
              previous = current
              b
            }
          }
        } else Iterator.empty
      }
    }

  def iterate[A](f: A => A, start: A): Evolution[A] =
    new Evolution[A] {
      override def run: Iterator[A] = Iterator.iterate(start)(f)
    }

  def iterate2[A](f: A => A => A, a0: A, a1: A): Evolution[A] =
    new Evolution[A] {
      def run: Iterator[A] = Iterator(a0, a1) ++ new AbstractIterator[A] {
        private var _prev = a0
        private var _current = a1
        def hasNext: Boolean = true
        def next(): A = {
          val current = f(_prev)(_current)
          _prev = _current
          _current = current
          current
        }
      }
    }

  def derive[A](as: Evolution[A], add: (A, A) => A, inverse: A => A): Evolution[A] =
    slidingMap[A, A](as, a1 => a2 => add(a2, inverse(a1)))

  def mapWithDerivative[A, B](as: Evolution[A], f: A => A => B, add: (A, A) => A, inverse: A => A): Evolution[B] =
    slidingMap[A, B](as, previous => current => f(previous)(add(current, inverse(previous))))

  def roll[A](f: Evolution[A => A], start: A): Evolution[A] =
    new Evolution[A] {
      def run: Iterator[A] = Iterator.single(start) ++ new AbstractIterator[A] {
        private val fIterator = f.run
        private var current = start
        def hasNext: Boolean = fIterator.hasNext
        def next(): A = {
          current = fIterator.next()(current)
          current
        }
      }
    }

  def roll2[A](f: Evolution[A => A => A], a0: A, a1: A): Evolution[A] =
    new Evolution[A] {
      def run: Iterator[A] = Iterator(a0, a1) ++ new AbstractIterator[A] {
        private val fIterator = f.run
        private var currentA0 = a0
        private var currentA1 = a1
        def hasNext: Boolean = fIterator.hasNext
        def next(): A = {
          val current = fIterator.next()(currentA0)(currentA1)
          currentA0 = currentA1
          currentA1 = current
          current
        }
      }
    }

  def solve1[A](speed: Evolution[A => A], start: A, add: (A, A) => A): Evolution[A] =
    roll(Evolution.map[A => A, A => A](speed, vFunc => a => add(a, vFunc(a))), start)

  def solve2[A](acc: Evolution[A => A => A], a0: A, v0: A, mult: (A, A) => A): Evolution[A] =
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
            _nextV = mult(currentV, accIterator.next()(currentA)(currentV))
            _nextA = mult(currentA, _nextV)
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
