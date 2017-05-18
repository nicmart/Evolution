package paint.evolution

import org.scalatest.{FlatSpec, Matchers, WordSpec}
import Evolution._
import Numeric._
import org.scalatest._
import paint.random.SequenceRNG

/**
  * Created by Nicolò Martini on 15/05/2017.
  */
class EvolutionTest extends PaintEvolutionSpec {

    "A slowed down evolution" must {

        "return the same evolution if slowed down by 1" in {
            assertEquivalent(int, int.slowDown(1))
        }

        "repeat twice the values when evolution slowed down by 2" in {
            val slowInts = int.slowDown(2)
            val expected = debug(int, 50)(Some(SequenceRNG(0))).flatMap(List.fill(2)(_))
            assertEvolution(slowInts, expected)
        }

        "repeat 10 times the values when evolution slowed down by 10" in {
            val slowInts = int.slowDown(10)
            val expected = debug(int, 100)(Some(SequenceRNG(0))).flatMap(List.fill(10)(_))
            assertEvolution(slowInts, expected)
        }

        "allows multiple slow downs" in {
            val expected = debug(int, 100)(Some(SequenceRNG(0))).flatMap(List.fill(4)(_))
            assertEvolution(int.slowDown(2).slowDown(2), expected)
        }
    }

    "A speeded up evolution" must {
        "return the same evolution if speed-up is zero" in {
            assertEquivalent(int, int.speedUp(0))
        }

        "discard elements" in {
            val expected = debug(int)(Some(SequenceRNG(0)))
                .filter { case (rng, n) => n % 3 == 0 }
            assertEvolution(int.speedUp(2), expected)
        }
    }

    "flatMap" must {

        "respect the unit law" in {
            assertEquivalent(int, int.flatMap(pure))
            assertEquivalent(pure(1), pure(1).flatMap(pure))
            assertEquivalent(double, double.flatMap(pure))
        }

        "respect composition law" in {
            val f1: Int => Evolution[Int] = n => pure(n + 1)
            val f2: Int => Evolution[Int] = n => pure(n * 2)
            assertEquivalent(
                int.flatMap(f1).flatMap(f2),
                int.flatMap(n => f1(n).flatMap(f2))
            )
        }
    }

    "map2" must {
        "be the same evolution generatedd by flatMap" in {
            val f: (Int, Int) => Int = (n, m) => n + 2 * m
            assertEquivalent(
                map2(f)(int, int),
                int.flatMap(n => int.map(m => f(n, m)))
            )
        }
    }

    "perturbate" must {

        "accumulate perturbations" in {
            // An evolution of perturbations s + x
            val perturbations = int.map[Int => Int] { n => m: Int => m + n }

            // p0 = 0 + x, int0 = 1, p1 = 2 + x, int1 = 3, p2 = 4 + x, int2 = 5
            // we expect accumulated perturbations 0 + x, 2 + x, 6 + x
            // obtaining 1, 5, 11
            assertValues(
                int.perturbate(perturbations),
                List(
                    1,
                    5,
                    11
                )
            )
        }
    }

    "filter" must {
        "mantain only the values that satisfy the condition" in {

            assertValues(
                int.filter(_ % 2 == 0),
                List(
                    0,
                    2,
                    4,
                    6,
                    8
                )
            )
        }
    }

    "sliding" must {
        "return a sliding evolution" in {
            assertValues(
                int.slidingPairs,
                List(
                    (0, 1),
                    (1, 2),
                    (2, 3)
                )
            )
        }
    }
}
