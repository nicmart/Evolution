package paint.laws

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import paint.evolution.algebra.EvolutionAlgebra

import scala.util.Random

//@todo remove W
trait LawsBaseSpec[Evo[+ _]]
  extends WordSpec
    with Matchers
    with PropertyChecks
    with EvolutionLaws[Evo] {
  val sampleSize = 10

  val E: EvolutionAlgebra[Evo]

  import E._

  def check[A](eq: IsEq[Evo[A]]): Unit

  "map compose" in {
    forAll(intEvolutions, intFunctions, intFunctions) { (evo, f, g) =>
      check(covariantComposition[Int, Int, Int](evo, f, g))
    }
  }

  "map is flatMap with pure" in {
    forAll(intEvolutions, intFunctions) { (evo, f) =>
      check(mapAsFlatmap(evo, f))
    }
  }

  "mapcons law 1" in {
    forAll(intEvolutions, intGen, intMapConsFunctions) { (evo, n, f) =>
      check(mapConsLaw1(evo, n, f))
    }
  }

  "mapcons law 2" in {
    forAll(intEvolutions) { (evo) =>
      check(mapConsLaw2(evo))
    }
  }

  "pure materializes to one-element stream" in {
    forAll(intGen) { (n) =>
      check(pureLaw(n))
    }
  }

//  "scan accumulates values" in {
//    forAll(intEvolutions, worlds) { (evo, w) =>
//      checkStream(scanLaw[Int, Int](evo, _ + _, 0, w))
//    }
//  }

  //  "int is a static evolution" in {
  //    // @TODO Not stack safe!
  //    forAll (Gen.choose(0, 100), Gen.choose(0, 100)) { (n, m) =>
  //      check(intIsAStaticEvolution(n, m))
  //    }
  //  }

  "repeat law" in {
    // @TODO Not stack safe!
    forAll(intEvolutions, nonNegativeInt) { (evo, n) =>
      if (n < Int.MaxValue / 1000) check(repeatLaw(evo, n))
    }
  }

  "slow down law" in {
    forAll(intEvolutions, intGen, nonNegativeInt) { (evo, m, n) =>
      check(slowDownLaw(evo, m, n))
    }
  }

  "filter law" in {
    forAll(intEvolutions, intGen, intPredicates) { (evo, n, p) =>
      check(filterLaw(evo, n, p))
    }
  }

  "sliding pairs law" in {
    forAll(intEvolutions, intGen, intGen) { (evo, n1, n2) =>
      check(slidingPairsLaw(evo, n1, n2))
    }
  }

  "grouped law" in {
    forAll(intEvolutions, nonNegativeInt) { (evo, n) =>
      check(groupedLaw(evo, n))
    }
  }

  def intGen: Gen[Int] =
    Gen.choose(Int.MinValue, Int.MaxValue)

  def nonNegativeInt: Gen[Int] =
    Gen.choose(0, Int.MaxValue)

  def intEvolutions: Gen[Evo[Int]] =
    Gen.oneOf(Seq(
      E.empty,
      pure(99),
      seq(List.fill(1000)(Random.nextInt()))
    )
    )

  def intFunctions: Gen[Int => Int] = intGen.flatMap { n =>
    Gen.oneOf[Int => Int](Seq[Int => Int](
      m => n * m,
      m => m + n,
      m => m * n * n + 1
    ))
  }

  def intMapConsFunctions: Gen[(Int, Evo[Int]) => Evo[Int]] = intGen.flatMap { n =>
    Gen.oneOf[(Int, Evo[Int]) => Evo[Int]](Seq[(Int, Evo[Int]) => Evo[Int]](
      (m, tail) => pure(m * n),
      (m, tail) => constant(n * m + 1),
      (m, tail) => tail
    ))
  }

  def intPredicates: Gen[Int => Boolean] =
    intGen.map(n => (m: Int) => n > m)

  def checkStream[A](eq: IsEq[Stream[A]]): Unit = {
    eq.lhs.take(sampleSize) shouldBe eq.rhs.take(sampleSize)
  }
}
