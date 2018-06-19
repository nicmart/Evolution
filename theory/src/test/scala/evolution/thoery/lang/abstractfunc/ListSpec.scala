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
    def constantStreamFlatmappedToEmpty[F[_], S[_]](alg: ListAlgebra[F, S]): F[Int] =
      flatMap(const(1)(alg), flatMapToEmpty[Int, Int])(alg)

    def recursiveStreamMappedToEmpty[F[_], S[_]](alg: ListAlgebra[F, S]): F[Int] =
      alg.fix(recursiveEmpty)

    def constantStreamFlatmappedToEmptyTwice[F[_], S[_]](alg: ListAlgebra[F, S]): F[Int] =
      flatMap(const(1)(alg), flatMapToFlatMapToEmpty[Int, Int])(alg)

    constantStreamFlatmappedToEmpty(StringInterpreter) shouldBe "flatMap(fix($self => 1::$self))(x => Nil)"
    constantStreamFlatmappedToEmpty(new MapEmpty.Annotator(StringInterpreter)).observe(StringInterpreter) shouldBe "Nil"

    constantStreamFlatmappedToEmptyTwice(StringInterpreter) shouldBe "flatMap(fix($self => 1::$self))(x => flatMap(x::Nil)(x => Nil))"
    constantStreamFlatmappedToEmptyTwice(new MapEmpty.Annotator(StringInterpreter))
      .observe(StringInterpreter) shouldBe "Nil"

    recursiveStreamMappedToEmpty(StringInterpreter) shouldBe "fix($self => Nil)"
    recursiveStreamMappedToEmpty(new MapEmpty.Annotator(StringInterpreter)).observe(StringInterpreter) shouldBe "Nil"

    val stream =
      constantStreamFlatmappedToEmpty(new MapEmpty.Annotator(LazyStreamInterpreter)).observe(LazyStreamInterpreter)()
    stream.take(10) shouldBe Stream.empty
  }

  def flatMap[A, B, F[_], S[_]](fa: F[A], f: SFFunc1[A, B])(alg: ListAlgebra[F, S]): F[B] =
    alg.flatMap(fa)(f)

  def const[A, F[_], S[_]](a: A)(alg: ListAlgebra[F, S]): F[A] = {
    alg.fix(new FFunc1[A, A] {
      override def run[F2[_], S2[_]](alg: ListAlgebra[F2, S2]): F2[A] => F2[A] =
        self => alg.cons(alg.value(a), self)
    })
  }

  def recursiveEmpty[A, B]: A FFunc1 B = new FFunc1[A, B] {
    override def run[F[_], S[_]](alg: ListAlgebra[F, S]): F[A] => F[B] =
      _ => alg.empty
  }

  def flatMapToEmpty[A, B]: SFFunc1[A, B] = new SFFunc1[A, B] {
    override def run[F[_], S[_]](alg: ListAlgebra[F, S]): S[A] => F[B] =
      _ => alg.empty
  }

  def flatMapToFlatMapToEmpty[A, B]: SFFunc1[A, B] = new SFFunc1[A, B] {
    override def run[F[_], S[_]](alg: ListAlgebra[F, S]): S[A] => F[B] =
      sa => alg.flatMap(alg.cons(sa, alg.empty))(flatMapToEmpty)
  }
}
