package evolution.materialization
import scala.collection.AbstractIterator
import scala.util.Random

trait Iterable[+T] {
  def run: Iterator[T]
}

object Iterable {
  val empty: Iterable[Nothing] = new Iterable[Nothing] {
    val run: Iterator[Nothing] = Iterator.empty
  }

  def constant[T](t: T): Iterable[T] = new Iterable[T] {
    val run: Iterator[T] = new AbstractIterator[T] {
      def hasNext = true
      def next = t
    }
  }

  def cons[T](head: T, tail: Iterable[T]): Iterable[T] = new Iterable[T] {
    def run: Iterator[T] = Iterator.single(head) ++ tail.run
  }

  def mapEmpty[T](it1: Iterable[T], it2: Iterable[T]): Iterable[T] = new Iterable[T] {
    def run: Iterator[T] = {
      val iterator1 = it1.run
      val iterator1HasNext = iterator1.hasNext
      if (iterator1HasNext) Iterator.single(iterator1.next()) ++ iterator1
      else it2.run
    }
  }

  def mapCons[A, B](it1: Iterable[A], f: A => Iterable[A] => Iterable[B]): Iterable[B] = new Iterable[B] {
    def run: Iterator[B] = {
      val iterator1 = it1.run
      val iterator1HasNext = iterator1.hasNext
      if (iterator1HasNext) {
        val head = iterator1.next()
        val tail = new Iterable[A] { val run = iterator1 }
        f(head)(tail).run
      } else empty.run
    }
  }

  def uniform(from: Double, to: Double): Iterable[Double] = new Iterable[Double] {
    def run: Iterator[Double] = Iterator.continually(from + Random.nextDouble() * (to - from))
  }

  def uniformFrom[T](n: Int, ts: Iterable[T]): Iterable[T] = new Iterable[T] {
    def run: Iterator[T] = {
      val materializedTs = ts.run.take(n).toList
      Iterator.continually(materializedTs(Random.nextInt(materializedTs.size)))
    }
  }

  def uniformDiscrete(from: Double, to: Double, step: Double): Iterable[Double] = new Iterable[Double] {
    private val size = ((to - from) / step).toInt + 1
    def run: Iterator[Double] = Iterator.continually(from + Random.nextInt(size) * step)
  }

  def normal(mu: Double, gamma: Double): Iterable[Double] = new Iterable[Double] {
    def run: Iterator[Double] = Iterator.continually(Random.nextGaussian() * gamma + mu)
  }
}
