package evolution.data
import cats.{ Group, Id }
import cats.arrow.FunctionK
import cats.kernel.{ Eq, Semigroup }
import evolution.geometry.Point
import evolution.typeclass.VectorSpace
import cats.~>

trait ExpressionModule[F[_]] {

  sealed abstract class Expr[T](val children: List[Expr[_]])

  object Expr {
    final case class Var[A](name: String) extends Expr[A](Nil)
    final case class Let[A, B](variable: String, value: Expr[A], expr: Expr[B]) extends Expr[B](List(value))
    final case class Lambda[A, B](variable: String, expr: Expr[B]) extends Expr[A => B](List(expr))
    final case class App[A, B](f: Expr[A => B], a: Expr[A]) extends Expr[B](List(f, a))
    final case class Fix[A](expr: Expr[A => A]) extends Expr[A](List(expr))

    final case class Dbl(d: Double) extends Expr[Double](Nil)
    final case class Floor(d: Expr[Double]) extends Expr[Int](List(d))
    final case class ToDbl(n: Expr[Int]) extends Expr[Double](List(n))
    final case class Integer(n: Int) extends Expr[Int](Nil)
    final case class Pnt(x: Expr[Double], y: Expr[Double]) extends Expr[Point](List(x, y))
    final case class X(p: Expr[Point]) extends Expr[Double](List(p))
    final case class Y(p: Expr[Point]) extends Expr[Double](List(p))
    final case class Add[T: Semigroup](a: Expr[T], b: Expr[T]) extends Expr[T](List(a, b)) {
      val semigroup: Semigroup[T] = implicitly[Semigroup[T]]
    }
    final case class Div(a: Expr[Double], b: Expr[Double]) extends Expr[Double](List(a, b))
    final case class Exp(a: Expr[Double], b: Expr[Double]) extends Expr[Double](List(a, b))
    final case class Abs(a: Expr[Double]) extends Expr[Double](List(a))
    final case class Sign(a: Expr[Double]) extends Expr[Double](List(a))
    final case class Mod(a: Expr[Double], b: Expr[Double]) extends Expr[Double](List(a, b))
    final case class Inverse[T: Group](t: Expr[T]) extends Expr[T](List(t)) {
      val group: Group[T] = implicitly[Group[T]]
    }
    final case class Multiply[T: VectorSpace](k: Expr[Double], t: Expr[T]) extends Expr[T](List(k, t)) {
      val vectorSpace: VectorSpace[T] = implicitly[VectorSpace[T]]
    }
    final case class Sin(d: Expr[Double]) extends Expr[Double](List(d))
    final case class Cos(d: Expr[Double]) extends Expr[Double](List(d))
    final case class Equals[T](a: Expr[T], b: Expr[T])(implicit val eq: Eq[T]) extends Expr[Boolean](List(a, b))
    final case class Neq[T](a: Expr[T], b: Expr[T])(implicit val eq: Eq[T]) extends Expr[Boolean](List(a, b))

    // Boolean
    final case class Bool(b: Boolean) extends Expr[Boolean](Nil)
    final case class IfThen[T](condition: Expr[Boolean], a: Expr[T], b: Expr[T]) extends Expr[T](List(condition, a, b))
    final case class InRect(topLeft: Expr[Point], bottomDown: Expr[Point], point: Expr[Point])
        extends Expr[Boolean](List(topLeft, bottomDown))
    final case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean](List(a, b))
    final case class Or(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean](List(a, b))
    final case class Not(a: Expr[Boolean]) extends Expr[Boolean](List(a))

    // Relations
    final case class GreaterThan[T](a: Expr[T], b: Expr[T])(implicit val ord: Ordering[T])
        extends Expr[Boolean](List(a, b))
    final case class GreaterThanOrEqual[T](a: Expr[T], b: Expr[T])(implicit val ord: Ordering[T])
        extends Expr[Boolean](List(a, b))
    final case class LessThan[T](a: Expr[T], b: Expr[T])(implicit val ord: Ordering[T])
        extends Expr[Boolean](List(a, b))
    final case class LessThanOrEqual[T](a: Expr[T], b: Expr[T])(implicit val ord: Ordering[T])
        extends Expr[Boolean](List(a, b))

    // Chain
    final case class Empty[A]() extends Expr[F[A]](Nil)
    final case class Cons[A](head: Expr[A], tail: Expr[F[A]]) extends Expr[F[A]](List(head, tail))
    final case class MapEmpty[A](eva: Expr[F[A]], eva2: Expr[F[A]]) extends Expr[F[A]](List(eva, eva2))
    final case class MapCons[A, B](eva: Expr[F[A]], f: Expr[A => F[A] => F[B]]) extends Expr[F[B]](List(eva, f))

    // Distributions
    final case class Uniform(from: Expr[Double], to: Expr[Double]) extends Expr[F[Double]](List(from, to))
    final case class UniformDiscrete(from: Expr[Double], to: Expr[Double], step: Expr[Double])
        extends Expr[F[Double]](List(from, to, step))
    final case class UniformFrom[T](n: Expr[Int], ft: Expr[F[T]]) extends Expr[F[T]](List(n, ft))
    final case class Normal(μ: Expr[Double], σ: Expr[Double]) extends Expr[F[Double]](List(μ, σ))

    def extractVariableNames(expr: Expr[_]): List[String] = expr match {
      case Var(name) => List(name)
      case _         => expr.children.flatMap(extractVariableNames)
    }

    def freshVar(expr: Expr[_], name: String): String = {
      val vars = extractVariableNames(expr).toSet
      def freshVarFromSuffix(n: Int): String = {
        if (vars.contains(s"$name$n")) freshVarFromSuffix(n + 1)
        else s"$name$n"
      }

      freshVarFromSuffix(0)
    }

    implicit class ExprOps[T](expr: Expr[T]) {
      def freshVarName(prefix: String): String = Expr.freshVar(expr, prefix)
      // TODO write a more performant version of these methods, where variables are extracted just once
      def freshVarName2(prefix1: String, prefix2: String): (String, String) =
        (Expr.freshVar(expr, prefix1), Expr.freshVar(expr, prefix2))
      def freshVarName3(prefix1: String, prefix2: String, prefix3: String): (String, String, String) =
        (Expr.freshVar(expr, prefix1), Expr.freshVar(expr, prefix2), Expr.freshVar(expr, prefix3))
      def freshVarName4(
        prefix1: String,
        prefix2: String,
        prefix3: String,
        prefix4: String): (String, String, String, String) =
        (
          Expr.freshVar(expr, prefix1),
          Expr.freshVar(expr, prefix2),
          Expr.freshVar(expr, prefix3),
          Expr.freshVar(expr, prefix4))
      def freshVar[S](prefix: String): Var[S] = Var(Expr.freshVar(expr, prefix))
    }
  }
}
