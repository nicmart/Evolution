package evolution.generator
import org.scalacheck.Gen

sealed trait Generator[T] {
  def underlying: Gen[T]
  def map[T2](f: T => T2): Generator[T2] = flatMap(f andThen Generator.pure)
  def flatMap[T2](f: T => Generator[T2]): Generator[T2]
}

object Generator {
  def pure[T](t: T): Generator[T] = Unknown(Gen.const(t))

  case class Fail[T]() extends Generator[T] {
    override def underlying: Gen[T] = Gen.fail
    override def flatMap[T2](f: T => Generator[T2]): Generator[T2] = Fail()
  }

  final case class Unknown[T](underlying: Gen[T]) extends Generator[T] {
    override def flatMap[T2](f: T => Generator[T2]): Generator[T2] = Unknown(underlying.flatMap(t => f(t).underlying))
  }

  sealed abstract case class Or[T](generators: List[Generator[T]]) extends Generator[T] {
    override def underlying: Gen[T] =
      if (generators.isEmpty) Gen.fail
      else Gen.choose(0, generators.size - 1).flatMap(i => generators(i).underlying)
    override def flatMap[T2](f: T => Generator[T2]): Generator[T2] = Or(generators.map(_.flatMap(f)))
  }

  object Or {
    def apply[T](generators: List[Generator[T]]): Generator[T] =
      new Or(generators.flatMap {
        case Or(innerGenerators) => innerGenerators
        case Fail() => Nil
        case generator => List(generator)
      }) {}
  }
}
