package evolution.data
import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.typeclass.VectorSpace

trait Initial[F[_]] {

  sealed trait R[T]
  final case class Dbl(d: Double) extends R[Double]
  final case class Floor(d: R[Double]) extends R[Int]
  final case class Integer(n: Int) extends R[Int]
  final case class Pnt(x: R[Double], y: R[Double]) extends R[Point]
  final case class X(p: R[Point]) extends R[Double]
  final case class Y(p: R[Point]) extends R[Double]
  final case class Add[T: Semigroup](a: R[T], b: R[T]) extends R[T] {
    val semigroup: Semigroup[T] = implicitly[Semigroup[T]]
    def map2(fa: R[T] => R[T], fb: R[T] => R[T]): Add[T] = Add(fa(a), fa(b))
  }
  final case class Div(a: R[Double], b: R[Double]) extends R[Double]
  final case class Exp(a: R[Double], b: R[Double]) extends R[Double]
  final case class Inverse[T: Group](t: R[T]) extends R[T] {
    val group: Group[T] = implicitly[Group[T]]
  }
  final case class Multiply[T: VectorSpace](k: R[Double], t: R[T]) extends R[T] {
    val vectorSpace: VectorSpace[T] = implicitly[VectorSpace[T]]
    def map2(fk: R[Double] => R[Double], ft: R[T] => R[T]): Multiply[T] = Multiply(fk(k), ft(t))
  }
  final case class Sin(d: R[Double]) extends R[Double]
  final case class Cos(d: R[Double]) extends R[Double]
  final case class Equals[T: Eq](a: R[T], b: R[T]) extends R[Boolean] {
    val eq: Eq[T] = implicitly[Eq[T]]
  }
  final case class IfThen[T](condition: R[Boolean], a: R[T], b: R[T]) extends R[T]

  final case class Var0[A](name: String) extends R[A]
  final case class Shift[A](expr: R[A]) extends R[A]
  final case class Let[A, B](variable: String, value: R[A], expr: R[B]) extends R[B]
  final case class Lambda[A, B](variable: String, expr: R[B]) extends R[A => B]
  final case class App[A, B](f: R[A => B], a: R[A]) extends R[B]
  final case class Fix[A](expr: R[A => A]) extends R[A]

  final case class Empty[A]() extends R[F[A]]
  final case class Cons[A](head: R[A], tail: R[F[A]]) extends R[F[A]]
  final case class MapEmpty[A](eva: R[F[A]], eva2: R[F[A]]) extends R[F[A]]
  final case class MapCons[A, B](eva: R[F[A]], f: R[A => F[A] => F[B]]) extends R[F[B]]

  final case class Uniform(from: R[Double], to: R[Double]) extends R[F[Double]]
  final case class UniformDiscrete(from: R[Double], to: R[Double], step: R[Double]) extends R[F[Double]]
  final case class UniformChoice[T](ts: List[R[T]]) extends R[F[T]]

  def VarN[A](n: Int, name: String): R[A] = {
    println(n -> name)
    if (n <= 0) Var0(name) else Shift(VarN(n - 1, name))
  }
}

trait WithInitial[F[_]] {
  final val initial = new Initial[F] {}
}
