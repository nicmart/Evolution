package evolution.thoery.lang.lambda

import evolution.theory.lang.lambda.{Alg, Printer}
import org.scalatest.{Matchers, WordSpec}

class LangSpec extends WordSpec with Matchers {
  import Expressions._

  "asdasd" should {
    "asdasd" in {
      flatMapToEmpty(Printer) shouldBe "flatMap(1::nil)(x -> nil)"
    }
  }

  object Expressions {
    def flatMapToEmpty[F[_], L[_]](alg: Alg[F, L]): F[L[Int]] = {
      import alg._
      flatMap(cons(int(1), nil))(lam(_ => nil))
    }
  }
}
