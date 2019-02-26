package evolution.data
import cats.{ Group, Id }
import cats.arrow.FunctionK
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import cats.~>

trait ExpressionModule[F[_]] {

  sealed trait Expr[T] {
    def transform(f: FunctionK[Expr, Expr]): Expr[T] = Expr.transform(f)(this)
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
  final case class Abs(a: Expr[Double]) extends Expr[Double]
  final case class Sign(a: Expr[Double]) extends Expr[Double]
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
  final case class Normal(μ: Expr[Double], σ: Expr[Double]) extends Expr[F[Double]]

  def VarN[A](n: Int, name: String): Expr[A] =
    if (n <= 0) Var0(name) else Shift(VarN(n - 1, name))

  object Expr {
    def transform[T](f: Expr ~> Expr): Expr ~> Expr =
      λ[Expr ~> Expr](fa => f(transformChildren(transform(f))(fa)))

    def transformChildren[T](f: Expr ~> Expr): Expr ~> Expr =
      λ[Expr ~> Expr] {
        case Dbl(d)                => Dbl(d)
        case Floor(d)              => Floor(f(d))
        case ToDbl(n)              => ToDbl(f(n))
        case Integer(n)            => Integer(n)
        case Pnt(x, y)             => Pnt(f(x), f(y))
        case X(p)                  => X(f(p))
        case Y(p)                  => Y(f(p))
        case add @ Add(a, b)       => Add(f(a), f(b))(add.semigroup)
        case Div(a, b)             => Div(f(a), f(b))
        case Exp(a, b)             => Exp(f(a), f(b))
        case Abs(a)                => Abs(f(a))
        case Sign(a)               => Sign(f(a))
        case Mod(a, b)             => Mod(f(a), f(b))
        case inverse @ Inverse(t)  => Inverse(f(t))(inverse.group)
        case mult @ Multiply(k, t) => Multiply(f(k), f(t))(mult.vectorSpace)
        case Sin(d)                => Sin(f(d))
        case Cos(d)                => Cos(f(d))
        case eq @ Equals(a, b)     => Equals(f(a), f(b))(eq.eq)
        case IfThen(condition, a, b) =>
          IfThen(f(condition), f(a), f(b))
        case Var0(name)                 => Var0(name)
        case Shift(expr)                => Shift(f(expr))
        case Let(variable, value, expr) => Let(variable, f(value), f(expr))
        case Lambda(variable, expr)     => Lambda(variable, f(expr))
        case App(func, a)               => App(f(func), f(a))
        case Fix(expr)                  => Fix(f(expr))
        case Empty()                    => Empty()
        case Cons(head, tail)           => Cons(f(head), f(tail))
        case MapEmpty(eva, eva2)        => MapEmpty(f(eva), f(eva2))
        case MapCons(eva, func)         => MapCons(f(eva), f(func))
        case Uniform(from, to)          => Uniform(f(from), f(to))
        case UniformDiscrete(from, to, step) =>
          UniformDiscrete(f(from), f(to), f(step))
        case UniformChoice(ts) => UniformChoice(ts.map(t => f(t)))
        case Normal(μ, σ)      => Normal(f(μ), f(σ))
      }

    def unshift[T](r: Expr[T]): Expr[T] = {
      val unshiftFK: Expr ~> Expr = λ[Expr ~> Expr] {
        case Shift(x) => x
        case t        => t
      }

      r.transform(unshiftFK)
    }
  }
}

trait WithExpression[FF[_]] {
  type F[T] = FF[T]
  final val expressionModule = new ExpressionModule[FF] {}
}
