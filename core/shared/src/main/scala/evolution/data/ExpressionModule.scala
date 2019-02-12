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
  final case class Integer(n: Int) extends Expr[Int]
  final case class Floor() extends Expr[Double => Int]

  final case class X() extends Expr[Point => Double]
  final case class Y() extends Expr[Point => Double]
  final case class Pnt() extends Expr[Double => Double => Point]
  final case class Add[T: Semigroup]() extends Expr[T => T => T] {
    val semigroup: Semigroup[T] = implicitly[Semigroup[T]]
  }
  final case class Div() extends Expr[Double => Double => Double]
  final case class Exp() extends Expr[Double => Double => Double]
  final case class Mod() extends Expr[Double => Double => Double]
  final case class Inverse[T: Group]() extends Expr[T => T] {
    val group: Group[T] = implicitly[Group[T]]
  }
  final case class Multiply[T: VectorSpace]() extends Expr[Double => T => T] {
    val vectorSpace: VectorSpace[T] = implicitly[VectorSpace[T]]
  }

  final case class Sin() extends Expr[Double => Double]
  final case class Cos() extends Expr[Double => Double]
  final case class Equals[T: Eq]() extends Expr[T => T => Boolean] {
    val eq: Eq[T] = implicitly[Eq[T]]
  }
  final case class IfThen[T]() extends Expr[Boolean => T => T => T]

  final case class Var0[A](name: String) extends Expr[A]
  final case class Shift[A](expr: Expr[A]) extends Expr[A]
  final case class Let[A, B](variable: String, value: Expr[A], expr: Expr[B]) extends Expr[B]
  final case class Lambda[A, B](variable: String, expr: Expr[B]) extends Expr[A => B]
  final case class App[A, B](f: Expr[A => B], a: Expr[A]) extends Expr[B]
  final case class Fix[A](f: Expr[A => A]) extends Expr[A]

  final case class Empty[A]() extends Expr[F[A]]
  final case class Cons[A](head: Expr[A], tail: Expr[F[A]]) extends Expr[F[A]]
  final case class MapEmpty[A](a: Expr[F[A]], b: Expr[F[A]]) extends Expr[F[A]]
  final case class MapCons[A, B](fa: Expr[F[A]], f: Expr[A => F[A] => F[B]]) extends Expr[F[B]]

  final case class Uniform() extends Expr[Double => Double => F[Double]]
  final case class UniformDiscrete() extends Expr[Double => Double => Double => F[Double]]
  final case class UniformChoice[T]() extends Expr[List[T] => F[T]]

  object Constructors {
    def add[T: Semigroup](a: Expr[T], b: Expr[T]): Expr[T] = App2(Add(), a, b)
    def inverse[T: Group](t: Expr[T]): Expr[T] = App(Inverse(), t)
    def multiply[T: VectorSpace](k: Expr[Double], v: Expr[T]): Expr[T] = App2(Multiply(), k, v)
    def pnt(x: Expr[Double], y: Expr[Double]): Expr[Point] = App2(Pnt(), x, y)
    def cos(alpha: Expr[Double]): Expr[Double] = App(Cos(), alpha)
    def sin(alpha: Expr[Double]): Expr[Double] = App(Sin(), alpha)
    def ifThen[T](condition: Expr[Boolean], ifTrue: Expr[T], ifFalse: Expr[T]): Expr[T] =
      App3[Boolean, T, T, T](IfThen(), condition, ifTrue, ifFalse)
    def equals[T: Eq](t1: Expr[T], t2: Expr[T]): Expr[Boolean] = App2(Equals(), t1, t2)
  }

  def VarN[A](n: Int, name: String): Expr[A] =
    if (n <= 0) Var0(name) else Shift(VarN(n - 1, name))

  def App2[A, B, C](f: Expr[A => B => C], a: Expr[A], b: Expr[B]): Expr[C] =
    App(App(f, a), b)

  def App3[A, B, C, D](f: Expr[A => B => C => D], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[D] =
    App(App(App(f, a), b), c)

  object Expr {
    def transform[T](r: Expr[T], f: FunctionK[Expr, Expr]): Expr[T] = f(transformChildren(r, f))

    def transformChildren[T](r: Expr[T], f: FunctionK[Expr, Expr]): Expr[T] = r match {
      case Shift(expr)                => Shift(transformChildren(expr, f))
      case Let(variable, value, expr) => Let(variable, transformChildren(value, f), transformChildren(expr, f))
      case Lambda(variable, expr)     => Lambda(variable, transformChildren(expr, f))
      case App(func, a)               => App(transformChildren(func, f), transformChildren(a, f))
      case Fix(g)                     => Fix(transformChildren(g, f))
      case Cons(h, t)                 => Cons(transformChildren(h, f), transformChildren(t, f))
      case MapEmpty(a, b)             => MapEmpty(transformChildren(a, f), transformChildren(b, f))
      case MapCons(a, g)              => MapCons(transformChildren(a, f), transformChildren(g, f))
      case _                          => r
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
