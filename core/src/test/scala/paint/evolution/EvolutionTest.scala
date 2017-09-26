package paint.evolution

import org.scalatest.{FlatSpec, Matchers, WordSpec}
import Evolution._
import NumericEvolutions._
import org.scalatest._
import paint.random.SequenceRNG

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

    "zipWith" must {
        "zip values of two evolutions" in {
            val f: (Int, Int) => Int = (n, m) => n + 2 * m
            assertValues(
                list(List(0, 1)).zipWith(list(List(0, 2, 3)))(f),
                List(0, 5)
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

    "grouped" must {
        "group elements by the provided argument" in {
            assertValues(
                int.grouped(3),
                List(
                    List(0, 1, 2),
                    List(3, 4, 5),
                    List(6, 7, 8)
                )
            )
        }
    }
}
