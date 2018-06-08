package evolution.thoery.lang.abstractfunc

import evolution.theory.lang.abstractfunc.{ListAlgebra, StreamInterpreter, StringInterpreter, ~>}
import org.scalatest.{FlatSpec, Matchers, WordSpec}

class ListSpec extends FlatSpec with Matchers {
  it should "apply fix operator" in {
    def const[F[_]](alg: ListAlgebra[F]): F[Int] = {
      alg.fix(new ~>[Int, Int] {
        override def run[G[_]](alg: ListAlgebra[G]): (() => G[Int]) => G[Int] = self => alg.cons(() => 1, self)
      })
    }

    val stream = const(StreamInterpreter)
    stream.take(10).toList shouldBe List.fill(10)(1)

    val serialized = const(StringInterpreter)
    serialized shouldBe "1::$self"
  }
}
