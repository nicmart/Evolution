package evolution.data
import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.derived.{ DefaultDerived, Derived }
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.Evolution
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

  val evolution: Evolution[F, R] = new Evolution[F, R] {
    override val chain: Chain[F, R] = new Chain[F, R] {
      override def empty[A]: R[F[A]] = Empty()
      override def cons[A](head: R[A], tail: R[F[A]]): R[F[A]] = Cons(head, tail)
      override def mapEmpty[A](eva: R[F[A]], eva2: R[F[A]]): R[F[A]] = MapEmpty(eva, eva2)
      override def mapCons[A, B](eva: R[F[A]])(f: R[A => F[A] => F[B]]): R[F[B]] = MapCons(eva, f)
    }

    override val constants: Constants[R] = new Constants[R] {
      override def int(n: Int): R[Int] = Integer(n)
      override def double(d: Double): R[Double] = Dbl(d)
      override def floor(d: R[Double]): R[Int] = Floor(d)
      override def point(x: R[Double], y: R[Double]): R[Point] = Pnt(x, y)
      override def x(point: R[Point]): R[Double] = X(point)
      override def y(point: R[Point]): R[Double] = Y(point)
      override def add[T: Semigroup](a: R[T], b: R[T]): R[T] = Add(a, b)
      override def div(a: R[Double], b: R[Double]): R[Double] = Div(a, b)
      override def exp(a: R[Double], b: R[Double]): R[Double] = Exp(a, b)
      override def inverse[T: Group](a: R[T]): R[T] = Inverse(a)
      override def multiply[T: VectorSpace](k: R[Double], t: R[T]): R[T] = Multiply(k, t)
      override def sin(d: R[Double]): R[Double] = Sin(d)
      override def cos(d: R[Double]): R[Double] = Cos(d)
      override def eq[T: Eq](a: R[T], b: R[T]): R[Boolean] = Equals(a, b)
      override def ifThen[T](condition: R[Boolean], a: R[T], b: R[T]): R[T] = IfThen(condition, a, b)
    }

    override val bind: Binding[R, String] = new Binding[R, String] {
      override def var0[A](name: String): R[A] = Var0(name)
      override def shift[A](expr: R[A]): R[A] = Shift(expr)
      override def let[A, B](variable: String, value: R[A], expr: R[B]): R[B] = Let(variable, value, expr)
      override def lambda[A, B](variable: String, expr: R[B]): R[A => B] = Lambda(variable, expr)
      override def app[A, B](f: R[A => B], a: R[A]): R[B] = App(f, a)
      override def fix[A](expr: R[A => A]): R[A] = Fix(expr)
    }
    override val distribution: Distribution[F, R] = new Distribution[F, R] {
      override def uniform(from: R[Double], to: R[Double]): R[F[Double]] = Uniform(from, to)
      override def uniformDiscrete(from: R[Double], to: R[Double], step: R[Double]): R[F[Double]] =
        UniformDiscrete(from, to, step)
      override def uniformChoice[T](ts: List[R[T]]): R[F[T]] = UniformChoice(ts)
    }
    override val derived: Derived[F, R] = new DefaultDerived[F, R](this)
  }
}

trait WithInitial[F[_]] {
  final val initial = new Initial[F] {}
}
