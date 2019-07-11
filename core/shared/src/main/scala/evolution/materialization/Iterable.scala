package evolution.materialization
import scala.collection.AbstractIterator
import scala.util.Random
import fastparse.utils.Generator.Iter

trait Iterable[+T] {
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

  def cons[T](head: T, tail: Iterable[T]): Iterable[T] = countAllocation(new Iterable[T] {
    def run: Iterator[T] = countRun(Iterator.single(head) ++ tail.run)
  })

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
        val it1 = fa.run
        val it2 = fb.run
        def hasNext = it1.hasNext && it2.hasNext
        def next = f(it1.next())(it2.next())
      })
    })
}
