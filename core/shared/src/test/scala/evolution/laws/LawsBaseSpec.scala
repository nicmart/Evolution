package evolution.laws

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import evolution.algebra.LegacyEvolutionAlgebra
import Arbitrary.arbitrary

import scala.util.Random

trait LawsBaseSpec[Evo[+ _]] extends WordSpec with Matchers with PropertyChecks with EvolutionLaws[Evo] {
  val sampleSize = 1000

  val E: LegacyEvolutionAlgebra[Evo]

  import E._

  def check[A](eq: IsEq[Evo[A]]): Unit

  "map compose" in {
    forAll(intEvolutions, arbitrary[Int => Int], arbitrary[Int => Int]) { (evo, f, g) =>
      check(covariantComposition[Int, Int, Int](evo, f, g))
    }
  }

  "map is flatMap with pure" in {
    forAll(intEvolutions, arbitrary[Int => Int]) { (evo, f) =>
      check(mapAsFlatmap(evo, f))
    }
  }

  "mapcons law 1" in {
    forAll(intEvolutions, arbitrary[Int], intMapConsFunctions) { (evo, n, f) =>
      check(mapConsLaw1(evo, n, f))
    }
  }

  "mapcons law 2" in {
    forAll(intEvolutions) { (evo) =>
      check(mapConsLaw2(evo))
    }
  }

  "pure materializes to one-element stream" in {
    forAll(arbitrary[Int]) { (n) =>
      check(pureLaw(n))
    }
  }

  "cons law" in {
    forAll { (n: Int) =>
      check(concatIsStackSafeLaw(n))
    }
  }

  "concat is stack safe" in {
    forAll(intEvolutions, nonNegativeInt) { (evo, n) =>
      check(consLaw(evo, n))
    }
  }

  "repeat law" in {
    forAll(intEvolutions, Gen.posNum[Int]) { (evo, n) =>
      check(repeatLaw(evo, n))
    }
  }

  "slow down law" in {
    forAll(intEvolutions, arbitrary[Int], nonNegativeInt) { (evo, m, n) =>
      check(slowDownLaw(evo, m, n))
    }
  }

  "filter law" in {
    forAll(intEvolutions, arbitrary[Int], arbitrary[Int => Boolean]) { (evo, n, p) =>
      check(filterLaw(evo, n, p))
    }
  }

  "sliding pairs law" in {
    forAll(intEvolutions, arbitrary[Int], arbitrary[Int]) { (evo, n1, n2) =>
      check(slidingPairsLaw(evo, n1, n2))
    }
  }

  def nonNegativeInt: Gen[Int] =
    Gen.choose(0, Int.MaxValue)

  def intEvolutions: Gen[Evo[Int]] =
    Gen.oneOf(Seq(E.empty, pure(99), seq(List.fill(1000)(Random.nextInt()))))

  def intMapConsFunctions: Gen[(Int, Evo[Int]) => Evo[Int]] = arbitrary[Int].flatMap { n =>
    Gen.oneOf[(Int, Evo[Int]) => Evo[Int]](
      Seq[(Int, Evo[Int]) => Evo[Int]]((m, tail) => pure(m * n), (m, tail) => constant(n * m + 1), (m, tail) => tail)
    )
  }

  def checkStream[A](eq: IsEq[Stream[A]]): Unit = {
    eq.lhs.take(sampleSize) shouldBe eq.rhs.take(sampleSize)
  }
}
