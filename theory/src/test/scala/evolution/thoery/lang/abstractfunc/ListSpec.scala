package evolution.thoery.lang.abstractfunc

import evolution.theory.lang.abstractfunc.Types.{Id, LazyStream}
import evolution.theory.lang.abstractfunc._
import org.scalatest.{FlatSpec, Matchers, WordSpec}

class ListSpec extends FlatSpec with Matchers {
  it should "apply fix operator" in {
    val stream = const(1)(LazyStreamInterpreter)()
    stream.take(10).toList shouldBe List.fill(10)(1)

    val serialized = const("1")(StringInterpreter)
    serialized shouldBe "fix($self => 1::$self)"
  }

  it should "flatmap" in {
    def emptyStream[F[_], S[_]](alg: ListAlgebra[F, S]): F[Int] =
      flatMap(const(1)(alg), flatMapToEmpty[Int, Int])(alg)
    //val stream = emptyStream(LazyStreamInterpreter)()
    //stream.take(10) shouldBe Stream.empty
    emptyStream(StringInterpreter) shouldBe "flatMap(fix($self => 1::$self))(x => Nil)"
  }

  def flatMap[A, B, F[_], S[_]](fa: F[A], f: FMap[A, B])(alg: ListAlgebra[F, S]): F[B] =
    alg.flatMap(fa)(f)

  def const[A, F[_], S[_]](a: A)(alg: ListAlgebra[F, S]): F[A] = {
    alg.fix(new ~>[A, A] {
      override def run[F2[_], S2[_]](alg: ListAlgebra[F2, S2]): F2[A] => F2[A] =
        self => alg.cons(alg.value(a), self)
    })
  }

  def flatMapToEmpty[A, B]: FMap[A, B] = new FMap[A, B] {
    override def run[F[_], S[_]](alg: ListAlgebra[F, S]): S[A] => F[B] =
      _ => alg.empty
  }
}
