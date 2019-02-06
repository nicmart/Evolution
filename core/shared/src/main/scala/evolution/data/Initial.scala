package evolution.data
import cats.{ Group, Id }
import cats.arrow.FunctionK
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.typeclass.VectorSpace

trait Initial[F[_]] {

  sealed trait R[T] {
    def transform(f: FunctionK[R, R]): R[T] = R.transform(this, f)
  }

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

  object R {
    def transform[T](r: R[T], f: FunctionK[R, R]): R[T] = f(transformChildren(r, f))

    def transformChildren[T](r: R[T], f: FunctionK[R, R]): R[T] = r match {
      case Dbl(d)                => Dbl(d)
      case Floor(d)              => Floor(transformChildren(d, f))
      case Integer(n)            => Integer(n)
      case Pnt(x, y)             => Pnt(transformChildren(x, f), transformChildren(y, f))
      case X(p)                  => X(transformChildren(p, f))
      case Y(p)                  => Y(transformChildren(p, f))
      case add @ Add(a, b)       => Add(transformChildren(a, f), transformChildren(b, f))(add.semigroup)
      case Div(a, b)             => Div(transformChildren(a, f), transformChildren(b, f))
      case Exp(a, b)             => Exp(transformChildren(a, f), transformChildren(b, f))
      case inverse @ Inverse(t)  => Inverse(transformChildren(t, f))(inverse.group)
      case mult @ Multiply(k, t) => Multiply(transformChildren(k, f), transformChildren(t, f))(mult.vectorSpace)
      case Sin(d)                => Sin(transformChildren(d, f))
      case Cos(d)                => Cos(transformChildren(d, f))
      case eq @ Equals(a, b)     => Equals(transformChildren(a, f), transformChildren(b, f))(eq.eq)
      case IfThen(condition, a, b) =>
        IfThen(transformChildren(condition, f), transformChildren(a, f), transformChildren(b, f))
      case Var0(name)                 => Var0(name)
      case Shift(expr)                => Shift(transformChildren(expr, f))
      case Let(variable, value, expr) => Let(variable, transformChildren(value, f), transformChildren(expr, f))
      case Lambda(variable, expr)     => Lambda(variable, transformChildren(expr, f))
      case App(func, a)               => App(transformChildren(func, f), transformChildren(a, f))
      case Fix(expr)                  => Fix(transformChildren(expr, f))
      case Empty()                    => Empty()
      case Cons(head, tail)           => Cons(transformChildren(head, f), transformChildren(tail, f))
      case MapEmpty(eva, eva2)        => MapEmpty(transformChildren(eva, f), transformChildren(eva2, f))
      case MapCons(eva, func)         => MapCons(transformChildren(eva, f), transformChildren(func, f))
      case Uniform(from, to)          => Uniform(transformChildren(from, f), transformChildren(to, f))
      case UniformDiscrete(from, to, step) =>
        UniformDiscrete(transformChildren(from, f), transformChildren(to, f), transformChildren(step, f))
      case UniformChoice(ts) => UniformChoice(ts.map(t => transformChildren(t, f)))
    }

    def unshift[T](r: R[T]): R[T] = {
      val unshiftFK: FunctionK[R, R] = new FunctionK[R, R] {
        def apply[T](t: R[T]): R[T] = t match {
          case Shift(x) => x
          case _        => t
        }
      }

      r.transform(unshiftFK)
    }
  }
}

trait WithInitial[F[_]] {
  final val initial = new Initial[F] {}
}
