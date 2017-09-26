package paint.laws

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import paint.evolution.algebra.{EvolutionAlgebra, MaterializableEvolutionAlgebra}

trait LawsBaseSpec[Evolution[_], W]
  extends WordSpec
  with Matchers
  with PropertyChecks
  with EvolutionLaws[Evolution, W]
{
  val sampleSize = 10

  val E: MaterializableEvolutionAlgebra[Evolution, W]
  def worlds: Gen[W]

  import E._

  "map compose" in {
    forAll (intEvolutions, intFunctions, intFunctions) { (evo, f, g) =>
      check(covariantComposition[Int, Int, Int](evo, f, g))
    }
  }

  "map is flatMap with pure" in {
    forAll (intEvolutions, intFunctions) { (evo, f) =>
      check(mapAsFlatmap(evo, f))
    }
  }

  "pure materializes to one-element stream" in {
    forAll (intGen, worlds) { (n, w) =>
      checkStream(pureLaw(n, w))
    }
  }

  "scan accumulates values" in {
    forAll (intEvolutions, worlds) { (evo, w) =>
      checkStream(scanLaw[Int, Int](evo, _ + _, 0, w))
    }
  }

  "int is a static evolution" in {
    // @TODO Not stack safe!
    forAll (Gen.choose(0, 100)) { n =>
      check(intIsAStaticEvolution(n))
    }
  }

  "repeat law" in {
    // @TODO Not stack safe!
    forAll (intEvolutions, nonNegativeInt) { (evo, n) =>
      if (n < Int.MaxValue / 1000) check(repeatLaw(evo, n))
    }
  }

  "slow down law" in {
    forAll(intEvolutions, nonNegativeInt, worlds) { (evo, n, w) =>
      checkStream(slowDownLaw(evo, n, w))
    }
  }

  "filter law" in {
    forAll (intEvolutions, intPredicates, worlds) { (evo, p, w) =>
      checkStream(filterLaw(evo, p, w))
    }
  }

  def intGen: Gen[Int] =
    Gen.choose(Int.MinValue, Int.MaxValue)

  def nonNegativeInt: Gen[Int] =
    Gen.choose(0, Int.MaxValue)

  def intEvolutions: Gen[Evolution[Int]] =
    Gen.oneOf(Seq(
      E.empty[Int],
      pure(99),
      int
    ))

  def intFunctions: Gen[Int => Int] = intGen.flatMap { n =>
    Gen.oneOf[Int => Int](Seq[Int => Int](
      m => n * m,
      m => m + n,
      m => m * n * n + 1
    ))
  }

  def intPredicates: Gen[Int => Boolean] =
    intGen.map(n => (m: Int) => n > m)

  def check[A](eq: IsEq[Evolution[A]]): Unit = {
    forAll (worlds) { (world: W) =>
      E.run(eq.lhs, world).take(sampleSize) shouldBe E.run(eq.rhs, world).take(sampleSize)
    }
  }

  def checkStream[A](eq: IsEq[Stream[A]]): Unit = {
    eq.lhs.take(100) shouldBe eq.rhs.take(100)
  }
}
