package paint.laws

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, WordSpec}
import paint.evolution.`new`.Evolutions

trait LawsBaseSpec[Evolution[_], W] extends WordSpec with Matchers with PropertyChecks {
  val sampleSize = 100

  val E: Evolutions[Evolution, W]
  def validWorlds: Gen[W]

  import E._

  def intGen: Gen[Int] =
    Gen.choose(Int.MinValue, Int.MaxValue)

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

  def check[A](eq: IsEq[Evolution[A]]): Unit = {
    forAll (validWorlds) { (world: W) =>
      E.run(eq.lhs, world).take(sampleSize) shouldBe E.run(eq.rhs, world).take(sampleSize)
    }
  }
}
