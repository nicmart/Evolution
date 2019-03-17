package evolution.materialization
import scala.annotation.tailrec

final case class RNGRepr[+A](
  run: RNG => (RNG, Option[(A, RNGRepr[A])])
) extends StateRepr[A, RNG, RNGRepr[A]] {
  override def self: RNGRepr[A] = this
}

object RNGRepr {
  def constant[T](t: T): RNGRepr[T] = {
    lazy val self: RNGRepr[T] = RNGRepr(rng => (rng, Some((t, self))))
    self
  }

  def map[A, B](ra: RNGRepr[A], f: A => B): RNGRepr[B] = RNGRepr { rng =>
    val (rng2, next) = ra.run(rng)
    next match {
      case None                   => (rng2, None)
      case Some((a, nextRNGRepr)) => (rng2, Some((f(a), map(nextRNGRepr, f))))
    }
  }

  def shuffle[T](ts: List[T]): RNGRepr[List[T]] = {
    lazy val self: RNGRepr[List[T]] = RNGRepr { rng =>
      val (shuffledTs, rng2) = shuffled(rng, ts.toVector)
      (rng2, Some((shuffledTs.toList, self)))
    }
    self
  }

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
