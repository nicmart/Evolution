package evolution.generator
import cats.Monad
import evolution.generator.Generator.Fail
import org.scalacheck.Gen

sealed trait Generator[T] {
  def underlying: Gen[T]
  def flatMap[T2](f: T => Generator[T2]): Generator[T2]
  def resize(newSize: Int => Int): Generator[T]
  def isFail: Boolean = this match {
    case Fail() => true
    case _ => false
  }
  def map[T2](f: T => T2): Generator[T2] = flatMap(f andThen Generator.pure)
}

object Generator {
  def pure[T](t: T): Generator[T] = Unknown(Gen.const(t))

  case class Fail[T]() extends Generator[T] {
    override def resize(newSize: Int => Int): Generator[T] = this
    override def underlying: Gen[T] = Gen.fail
    override def flatMap[T2](f: T => Generator[T2]): Generator[T2] = Fail()
  }

  final case class Unknown[T](underlying: Gen[T]) extends Generator[T] {
    override def flatMap[T2](f: T => Generator[T2]): Generator[T2] =
      Unknown(underlying.flatMap(t => f(t).underlying))
    override def resize(newSize: Int => Int): Generator[T] =
      Unknown(Gen.sized(s => Gen.resize(newSize(s), underlying)))
  }

  sealed abstract case class Or[T](generators: List[Generator[T]]) extends Generator[T] {
    override def underlying: Gen[T] =
      if (generators.isEmpty) Gen.fail
      else
        Gen.choose(0, generators.size - 1).flatMap(i => generators(i).underlying).map { t =>
          //println(s"Or generator of size ${nonFailGenerators.size} generated value $t")
          t
        }

    override def flatMap[T2](f: T => Generator[T2]): Generator[T2] = Or(generators.map(_.flatMap(f)))
    override def resize(newSize: Int => Int): Generator[T] = Or(generators.map(_.resize(newSize)))
  }

  object Or {
    def apply[T](generators: List[Generator[T]]): Generator[T] =
      new Or(generators.flatMap {
        case Or(innerGenerators) => innerGenerators
        case Fail() =>
          Nil
        case generator => List(generator)
      }) {}
  }

  implicit val monadInstance: Monad[Generator] = new Monad[Generator] {
    override def pure[A](x: A): Generator[A] = Generator.pure(x)
    override def flatMap[A, B](fa: Generator[A])(f: A => Generator[B]): Generator[B] = fa.flatMap(f)
    // TODO Stack unsafe. See https://github.com/non/cats-check/blob/master/src/main/scala/catscheck/instances/gen.scala
    override def tailRecM[A, B](a: A)(f: A => Generator[Either[A, B]]): Generator[B] = flatMap(f(a)) {
      case Left(a2) => tailRecM(a2)(f)
      case Right(b) => pure(b)
    }
  }
}
