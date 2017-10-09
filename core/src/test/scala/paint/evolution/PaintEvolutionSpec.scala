package paint.evolution

import org.scalatest.{Matchers, WordSpec}
import paint.random.{RNG, SequenceRNG}

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
trait PaintEvolutionSpec extends WordSpec with Matchers {

  import Evolution._

  def assertEquivalent[A](ev1: Evolution[A], ev2: Evolution[A], size: Int = 50): Unit = {
    val rng = SequenceRNG(0)
    val sample1 = debug(ev1, size)(Some(rng))
    val sample2 = debug(ev2, size)(Some(rng))
    sample1 shouldBe sample2
  }

  def assertEvolution[A](ev: Evolution[A], expected: List[(RNG, A)]): Unit = {
    val rng = SequenceRNG(0)
    val sample = debug(ev, expected.length)(Some(rng))
    sample shouldBe expected
  }

  def assertSameValues[A](ev1: Evolution[A], ev2: Evolution[A], size: Int = 50): Unit = {
    val rng = SequenceRNG(0)
    val sample1 = sample(ev1, 50, Some(rng))
    val sample2 = sample(ev2, 50, Some(rng))
    sample1 shouldBe sample2
  }

  def assertValues[A](ev: Evolution[A], expected: List[A]): Unit = {
    val rng = SequenceRNG(0)
    val sample = Evolution.sample(ev, expected.length, Some(rng))
    sample shouldBe expected
  }
}
