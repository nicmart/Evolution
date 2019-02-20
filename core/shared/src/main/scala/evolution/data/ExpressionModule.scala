package evolution.data
import cats.{ Group, Id }
import cats.arrow.FunctionK
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.typeclass.VectorSpace

trait ExpressionModule[F[_]] {

  sealed trait Expr[T] {
    def transform(f: FunctionK[Expr, Expr]): Expr[T] = Expr.transform(this, f)
  }

  // We are using case classes when we could have used objects because the pattern matching was failing with objects
  // in the interpreter module!
  final case class Dbl(d: Double) extends Expr[Double]
  final case class Floor(d: Expr[Double]) extends Expr[Int]
  final case class ToDbl(n: Expr[Int]) extends Expr[Double]
  final case class Integer(n: Int) extends Expr[Int]
  final case class Pnt(x: Expr[Double], y: Expr[Double]) extends Expr[Point]
  final case class X(p: Expr[Point]) extends Expr[Double]
  final case class Y(p: Expr[Point]) extends Expr[Double]
  final case class Add[T: Semigroup](a: Expr[T], b: Expr[T]) extends Expr[T] {
    val semigroup: Semigroup[T] = implicitly[Semigroup[T]]
  }
  final case class Div(a: Expr[Double], b: Expr[Double]) extends Expr[Double]
  final case class Exp(a: Expr[Double], b: Expr[Double]) extends Expr[Double]
  final case class Mod(a: Expr[Double], b: Expr[Double]) extends Expr[Double]
  final case class Inverse[T: Group](t: Expr[T]) extends Expr[T] {
    val group: Group[T] = implicitly[Group[T]]
  }
  final case class Multiply[T: VectorSpace](k: Expr[Double], t: Expr[T]) extends Expr[T] {
    val vectorSpace: VectorSpace[T] = implicitly[VectorSpace[T]]
  }
  final case class Sin(d: Expr[Double]) extends Expr[Double]
  final case class Cos(d: Expr[Double]) extends Expr[Double]
  final case class Equals[T: Eq](a: Expr[T], b: Expr[T]) extends Expr[Boolean] {
    val eq: Eq[T] = implicitly[Eq[T]]
  }
  final case class IfThen[T](condition: Expr[Boolean], a: Expr[T], b: Expr[T]) extends Expr[T]

  final case class Var0[A](name: String) extends Expr[A]
  final case class Shift[A](expr: Expr[A]) extends Expr[A]
  final case class Let[A, B](variable: String, value: Expr[A], expr: Expr[B]) extends Expr[B]
  final case class Lambda[A, B](variable: String, expr: Expr[B]) extends Expr[A => B]
  final case class App[A, B](f: Expr[A => B], a: Expr[A]) extends Expr[B]
  final case class Fix[A](expr: Expr[A => A]) extends Expr[A]

  final case class Empty[A]() extends Expr[F[A]]
  final case class Cons[A](head: Expr[A], tail: Expr[F[A]]) extends Expr[F[A]]
  final case class MapEmpty[A](eva: Expr[F[A]], eva2: Expr[F[A]]) extends Expr[F[A]]
  final case class MapCons[A, B](eva: Expr[F[A]], f: Expr[A => F[A] => F[B]]) extends Expr[F[B]]

  final case class Uniform(from: Expr[Double], to: Expr[Double]) extends Expr[F[Double]]
  final case class UniformDiscrete(from: Expr[Double], to: Expr[Double], step: Expr[Double]) extends Expr[F[Double]]
  final case class UniformChoice[T](ts: List[Expr[T]]) extends Expr[F[T]]

  def VarN[A](n: Int, name: String): Expr[A] =
    if (n <= 0) Var0(name) else Shift(VarN(n - 1, name))

  object Expr {
    def transform[T](r: Expr[T], f: FunctionK[Expr, Expr]): Expr[T] = f(transformChildren(r, f))

    def transformChildren[T](r: Expr[T], f: FunctionK[Expr, Expr]): Expr[T] = r match {
      case Dbl(d)                => Dbl(d)
      case Floor(d)              => Floor(transformChildren(d, f))
      case ToDbl(n)              => ToDbl(transformChildren(n, f))
      case Integer(n)            => Integer(n)
      case Pnt(x, y)             => Pnt(transformChildren(x, f), transformChildren(y, f))
      case X(p)                  => X(transformChildren(p, f))
      case Y(p)                  => Y(transformChildren(p, f))
      case add @ Add(a, b)       => Add(transformChildren(a, f), transformChildren(b, f))(add.semigroup)
      case Div(a, b)             => Div(transformChildren(a, f), transformChildren(b, f))
      case Exp(a, b)             => Exp(transformChildren(a, f), transformChildren(b, f))
      case Mod(a, b)             => Mod(transformChildren(a, f), transformChildren(b, f))
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

    def unshift[T](r: Expr[T]): Expr[T] = {
      val unshiftFK: FunctionK[Expr, Expr] = new FunctionK[Expr, Expr] {
        def apply[U](t: Expr[U]): Expr[U] = t match {
          case Shift(x) => x
          case _        => t
        }
      }

      r.transform(unshiftFK)
    }
  }
}

trait WithExpression[FF[_]] {
  type F[T] = FF[T]
  final val expressionModule = new ExpressionModule[FF] {}
}
