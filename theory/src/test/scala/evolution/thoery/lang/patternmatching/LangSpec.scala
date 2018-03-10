package evolution.thoery.lang.patternmatching

import evolution.theory.lang.patternmatching.{Const, Serializer}
import org.scalatest.{Matchers, WordSpec}

class LangSpec extends WordSpec with Matchers {
  import evolution.theory.lang.patternmatching.All._
  "The serializer interpreter" should {
    "serialize expressions properly" in {
      import evolution.theory.lang.patternmatching.Builder._
      val expression = add(
        map(map(pure(1))(_ * 2))(_ + 1),
        map(add(pure(3), pure(4)))(_ * 3)
      )
      expression.run[λ[X => String]](Serializer) shouldBe "add(map(map(pure(1))(?))(?), map(add(pure(3), pure(4)))(?))"
    }
  }

  "The optimised serializer interpreter" should {
    "serialize expressions after optimisations" in {
      import evolution.theory.lang.patternmatching.Builder._
      val expression = add(
        map(map(pure(1))(_ * 2))(_ + 1),
        map(add(pure(3), pure(4)))(_ * 3)
      )
      optimise(expression).run[λ[X => String]](Serializer) shouldBe "pure(24)"
    }
  }
}
