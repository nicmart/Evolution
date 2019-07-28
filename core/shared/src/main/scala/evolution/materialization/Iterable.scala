package evolution.materialization
import cats.kernel.{ Group, Semigroup }

import scala.collection.AbstractIterator
import scala.util.Random
import evolution.geometry.Point
import evolution.rng.PerlinNoise
import evolution.typeclass.VectorSpace

import scala.collection.mutable.ArrayBuffer

sealed trait Iterable[+T] {
  def run: Iterator[T]
}

object Iterable {
  private var allocations = 0
  private var runs = 0

  def resetCounts(): Unit = {
    allocations = 0
    runs = 0
  }
  def allocationCount = allocations
  def runsCount = runs

  private def countAllocation[T](t: T): T = {
    allocations += 1
    t
  }

  private def countRun[T](t: T): T = {
    runs += 1
    t
  }

  val empty: Iterable[Nothing] = countAllocation(new Iterable[Nothing] {
    val run: Iterator[Nothing] = countRun(Iterator.empty)
  })

  def constant[T](t: T): Iterable[T] = countAllocation(new Iterable[T] {
    val run: Iterator[T] = countRun(new AbstractIterator[T] {
      def hasNext = true
      def next = t
    })
  })

  def cons[T](head: T, tail: => Iterable[T]): Iterable[T] = countAllocation(new Iterable[T] {
    def run: Iterator[T] = countRun(Iterator.single(head) ++ tail.run)
  })

  def concat[T](it1: Iterable[T], it2: Iterable[T]): Iterable[T] = countAllocation(
    new Iterable[T] {
      def run: Iterator[T] = countRun(it1.run ++ it2.run)
    }
  )

  def mapEmpty[T](it1: Iterable[T], it2: Iterable[T]): Iterable[T] = countAllocation(new Iterable[T] {
    def run: Iterator[T] = countRun {
      val iterator1 = it1.run
      val iterator1HasNext = iterator1.hasNext
      if (iterator1HasNext) Iterator.single(iterator1.next()) ++ iterator1
      else it2.run
    }
  })

  def mapCons[A, B](it1: Iterable[A], f: A => Iterable[A] => Iterable[B]): Iterable[B] =
    countAllocation(new Iterable[B] {
      def run: Iterator[B] = countRun {
        val iterator1 = it1.run
        val iterator1HasNext = iterator1.hasNext
        if (iterator1HasNext) {
          val head = iterator1.next()
          val tail = new Iterable[A] { val run = iterator1 }
          f(head)(tail).run
        } else empty.run
      }
    })

  def range(start: Double, end: Double, step: Double): Iterable[Double] = countAllocation(
    new Iterable[Double] {
      def run: Iterator[Double] = countRun(
        (start to end by step).toIterator
      )
    }
  )

  def uniform(from: Double, to: Double): Iterable[Double] = countAllocation(new Iterable[Double] {
    def run: Iterator[Double] = countRun(Iterator.continually(from + Random.nextDouble() * (to - from)))
  })

  def uniformFrom[T](n: Int, ts: Iterable[T]): Iterable[T] = countAllocation(new Iterable[T] {
    def run: Iterator[T] = countRun {
      val materializedTs = ts.run.take(n).toList
      Iterator.continually(materializedTs(Random.nextInt(materializedTs.size)))
    }
  })

  def uniformDiscrete(from: Double, to: Double, step: Double): Iterable[Double] =
    countAllocation(new Iterable[Double] {
      private val size = ((to - from) / step).toInt + 1
      def run: Iterator[Double] = countRun(Iterator.continually(from + Random.nextInt(size) * step))
    })

  def normal(mu: Double, gamma: Double): Iterable[Double] = countAllocation(new Iterable[Double] {
    def run: Iterator[Double] = countRun(Iterator.continually(Random.nextGaussian() * gamma + mu))
  })

  def zipWith[A, B, C](fa: Iterable[A], fb: Iterable[B], f: A => B => C): Iterable[C] =
    countAllocation(new Iterable[C] {
      def run: Iterator[C] = countRun(new AbstractIterator[C] {
        private val it1 = fa.run
        private val it2 = fb.run
        def hasNext = it1.hasNext && it2.hasNext
        def next = f(it1.next())(it2.next())
      })
    })

  def zipWithUncurried[A, B, C](f: (A, B) => C)(fa: Iterable[A], fb: Iterable[B]): Iterable[C] =
    countAllocation(new Iterable[C] {
      def run: Iterator[C] = countRun(new AbstractIterator[C] {
        val it1 = fa.run
        val it2 = fb.run
        def hasNext = it1.hasNext && it2.hasNext
        def next = f(it1.next(), it2.next())
      })
    })

  def take[A](n: Int, fa: Iterable[A]): Iterable[A] = countAllocation(new Iterable[A] {
    def run: Iterator[A] = countRun(fa.run.take(n))
  })

  def takeWhile[A](fa: Iterable[A], predicate: A => Boolean): Iterable[A] = countAllocation(new Iterable[A] {
    def run: Iterator[A] = countRun(fa.run.takeWhile(predicate))
  })

  def map[A, B](fa: Iterable[A], f: A => B): Iterable[B] = countAllocation(new Iterable[B] {
    def run: Iterator[B] = countRun(fa.run.map(f))
  })

  def flatMap[A, B](fa: Iterable[A], f: A => Iterable[B]): Iterable[B] = countAllocation(new Iterable[B] {
    def run: Iterator[B] = countRun(fa.run.flatMap(a => f(a).run))
  })

  def flatten[A, B](fa: Iterable[Iterable[A]]): Iterable[A] = countAllocation(new Iterable[A] {
    def run: Iterator[A] = countRun(fa.run.flatMap(_.run))
  })

  def parallel[A](ffa: Iterable[Iterable[A]]): Iterable[A] = countAllocation(new Iterable[A] {
    override def run: Iterator[A] = countRun(
      new AbstractIterator[A] {
        private val iteratorOfIterables = ffa.run
        private val iterators: ArrayBuffer[Iterator[A]] = ArrayBuffer.empty
        private var currentIndex = 0
        private var outerIterationEnded = false
        override def hasNext: Boolean = {
          if (outerIterationEnded) iterators(currentIndex).hasNext
          else {
            if (iteratorOfIterables.hasNext) {
              iterators.append(iteratorOfIterables.next().run)
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
    )
  })

  def integrate[A](start: A, speed: Iterable[A], semigroup: Semigroup[A]): Iterable[A] = countAllocation(
    new Iterable[A] {
      def run: Iterator[A] = countRun(new AbstractIterator[A] {
        private val speedIterator = speed.run
        private var _hasNext = true
        private var _next = start
        def hasNext: Boolean = _hasNext
        def next(): A = {
          val current = _next
          if (speedIterator.hasNext) {
            _hasNext = true
            _next = semigroup.combine(current, speedIterator.next())
          } else {
            _hasNext = false
          }

          current
        }
      })
    }
  )

  def derive[A](as: Iterable[A], group: Group[A]): Iterable[A] = countAllocation(
    new Iterable[A] {
      override def run: Iterator[A] = countRun {
        val derivingIterator: Iterator[A] = as.run
        if (derivingIterator.hasNext) {
          new AbstractIterator[A] {
            var _current: A = derivingIterator.next()
            override def hasNext: Boolean = derivingIterator.hasNext
            override def next(): A = {
              val nextA = derivingIterator.next()
              val nextDerivative = group.remove(nextA, _current)
              _current = nextA
              nextDerivative
            }
          }
        } else Iterator.empty
      }
    }
  )

  // TODO DRY with derivative?
  def mapWithDerivative[A, B](as: Iterable[A], f: A => A => B, group: Group[A]): Iterable[B] = countAllocation(
    new Iterable[B] {
      override def run: Iterator[B] = countRun {
        val derivingIterator: Iterator[A] = as.run
        if (derivingIterator.hasNext) {
          new AbstractIterator[B] {
            var _current: A = derivingIterator.next()
            override def hasNext: Boolean = derivingIterator.hasNext
            override def next(): B = {
              val nextA = derivingIterator.next()
              val nextB = f(_current)(group.remove(nextA, _current))
              _current = nextA
              nextB
            }
          }
        } else Iterator.empty
      }
    }
  )

  // TODO DRY, see integrate
  def solve1[A](speed: Iterable[A => A], start: A, semigroup: Semigroup[A]): Iterable[A] = countAllocation(
    new Iterable[A] {
      def run: Iterator[A] = countRun(new AbstractIterator[A] {
        private val speedIterator = speed.run
        private var _hasNext = true
        private var _next = start
        def hasNext: Boolean = _hasNext
        def next(): A = {
          val current = _next
          if (speedIterator.hasNext) {
            _hasNext = true
            _next = semigroup.combine(current, speedIterator.next()(current))
          } else {
            _hasNext = false
          }

          current
        }
      })
    }
  )

  def solve2[A](acc: Iterable[A => A => A], a0: A, v0: A, semigroup: Semigroup[A]): Iterable[A] = countAllocation(
    new Iterable[A] {
      def run: Iterator[A] = countRun(new AbstractIterator[A] {
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
            _nextV = semigroup.combine(currentV, accIterator.next()(currentA)(currentV))
            _nextA = semigroup.combine(currentA, _nextV)
          } else {
            _hasNext = false
          }

          currentA
        }
      })
    }
  )

  def withFirst1[A, B](ts: Iterable[A], f: A => Iterable[B]): Iterable[B] = countAllocation(new Iterable[B] {
    override def run: Iterator[B] = countRun(
      ts.run.take(1).toList match {
        case a1 :: Nil => f(a1).run
        case _         => Iterator.empty
      }
    )
  })

  def withFirst2[A, B](ts: Iterable[A], f: A => A => Iterable[B]): Iterable[B] = countAllocation(new Iterable[B] {
    override def run: Iterator[B] = countRun(
      ts.run.take(2).toList match {
        case a1 :: a2 :: Nil => f(a1)(a2).run
        case _               => Iterator.empty
      }
    )
  })

  def withFirst3[A, B](ts: Iterable[A], f: A => A => A => Iterable[B]): Iterable[B] = countAllocation(new Iterable[B] {
    override def run: Iterator[B] = countRun(
      ts.run.take(3).toList match {
        case a1 :: a2 :: a3 :: Nil => f(a1)(a2)(a3).run
        case _                     => Iterator.empty
      }
    )
  })

  def shuffle[T](ts: List[T]): Iterable[List[T]] = countAllocation(new Iterable[List[T]] {
    def run: Iterator[List[T]] = countRun(Iterator.continually(Random.shuffle(ts)))
  })

  private val range: List[Int] = (0 to 255).toList

  val noiseIterable: Iterable[Point => Double] =
    map(
      shuffle(range),
      (permutation: List[Int]) => {
        val perlinNoise = new PerlinNoise(permutation.toArray)
        point => perlinNoise.noise(point.x, point.y)
      }
    )

  val octaveNoiseIterable: Iterable[Int => Double => Point => Double] =
    map(
      shuffle(range),
      (permutation: List[Int]) => {
        val perlinNoise = new PerlinNoise(permutation.toArray)
        octaves => presistence => point => perlinNoise.octaveNoise(octaves, presistence, point.x, point.y)
      }
    )
}
