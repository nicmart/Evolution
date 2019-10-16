package evolution.compiler.expression
import evolution.geometry.Point
import evolution.materialization.Evolution
import evolution.compiler.expression.typeclass._

sealed abstract class Expr[+T](val children: List[Expr[_]])

object Expr {
  final case class Var[A](name: String) extends Expr[A](Nil)
  final case class Let[A, B](variable: String, value: Expr[A], expr: Expr[B]) extends Expr[B](List(value))
  final case class Lambda[-A, +B](variable: String, expr: Expr[B]) extends Expr[A => B](List(expr))
  final case class App[A, B](f: Expr[A => B], a: Expr[A]) extends Expr[B](List(f, a))
  final case class Fix[A](expr: Expr[A => A]) extends Expr[A](List(expr))

  final case class Dbl(d: Double) extends Expr[Double](Nil)
  final case class Floor(d: Expr[Double]) extends Expr[Int](List(d))
  final case class ToDouble(n: Expr[Int]) extends Expr[Double](List(n))
  final case class Integer(n: Int) extends Expr[Int](Nil)
  final case class Pnt(x: Expr[Double], y: Expr[Double]) extends Expr[Point](List(x, y))
  final case class LiftedPnt(x: Expr[Evolution[Double]], y: Expr[Evolution[Double]])
      extends Expr[Evolution[Point]](List(x, y))
  final case class Polar(r: Expr[Double], alpha: Expr[Double]) extends Expr[Point](List(r, alpha))
  final case class LiftedPolar(r: Expr[Evolution[Double]], alpha: Expr[Evolution[Double]])
      extends Expr[Evolution[Point]](List(r, alpha))
  final case class X(p: Expr[Point]) extends Expr[Double](List(p))
  final case class Y(p: Expr[Point]) extends Expr[Double](List(p))
  final case class Norm(p: Expr[Point]) extends Expr[Double](List(p))
  final case class Versor(p: Expr[Point]) extends Expr[Point](List(p))
  final case class Add[A, B, C](a: Expr[A], b: Expr[B], add: Additive[A, B, C]) extends Expr[C](List(a, b))
  final case class Div(a: Expr[Double], b: Expr[Double]) extends Expr[Double](List(a, b))
  final case class Exp(a: Expr[Double], b: Expr[Double]) extends Expr[Double](List(a, b))
  final case class Abs(a: Expr[Double]) extends Expr[Double](List(a))
  final case class Sign(a: Expr[Double]) extends Expr[Double](List(a))
  final case class Mod(a: Expr[Double], b: Expr[Double]) extends Expr[Double](List(a, b))
  final case class Inverse[T](t: Expr[T], inv: Invertible[T]) extends Expr[T](List(t))
  final case class Minus[T](a: Expr[T], b: Expr[T], sg: Additive[T, T, T], inv: Invertible[T])
      extends Expr[T](List(a, b))
  final case class Derive[T](t: Expr[Evolution[T]], sg: Additive[T, T, T], inv: Invertible[T])
      extends Expr[Evolution[T]](List(t))
  final case class Multiply[A, B, C](a: Expr[A], b: Expr[B], mult: Multiplicative[A, B, C]) extends Expr[C](List(a, b))
  final case class Sin(d: Expr[Double]) extends Expr[Double](List(d))
  final case class Cos(d: Expr[Double]) extends Expr[Double](List(d))
  final case class Lst[T](ts: List[Expr[T]]) extends Expr[List[T]](ts)
  final case class SmoothStep(from: Expr[Double], to: Expr[Double], position: Expr[Double])
      extends Expr[Double](List(from, to, position))

  final case class Equals[T](a: Expr[T], b: Expr[T], eq: Equable[T]) extends Expr[Boolean](List(a, b))
  final case class Neq[T](a: Expr[T], b: Expr[T], eq: Equable[T]) extends Expr[Boolean](List(a, b))

  // Boolean
  final case class Bool(b: Boolean) extends Expr[Boolean](Nil)
  final case class IfThen[T](condition: Expr[Boolean], a: Expr[T], b: Expr[T]) extends Expr[T](List(condition, a, b))
  final case class InRect(topLeft: Expr[Point], bottomDown: Expr[Point], point: Expr[Point])
      extends Expr[Boolean](List(topLeft, bottomDown))
  final case class And(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean](List(a, b))
  final case class Or(a: Expr[Boolean], b: Expr[Boolean]) extends Expr[Boolean](List(a, b))
  final case class Not(a: Expr[Boolean]) extends Expr[Boolean](List(a))

  // Relations
  final case class GreaterThan[T](a: Expr[T], b: Expr[T], cmp: Comparable[T]) extends Expr[Boolean](List(a, b))
  final case class GreaterThanOrEqual[T](a: Expr[T], b: Expr[T], cmp: Comparable[T]) extends Expr[Boolean](List(a, b))
  final case class LessThan[T](a: Expr[T], b: Expr[T], cmp: Comparable[T]) extends Expr[Boolean](List(a, b))
  final case class LessThanOrEqual[T](a: Expr[T], b: Expr[T], cmp: Comparable[T]) extends Expr[Boolean](List(a, b))

  // Chain
  final case class FromList[T](ts: Expr[List[T]]) extends Expr[Evolution[T]](List(ts))
  final case class Empty[A]() extends Expr[Evolution[A]](Nil)
  final case class Cons[A](head: Expr[A], tail: Expr[Evolution[A]]) extends Expr[Evolution[A]](List(head, tail))
  final case class Concat[A](as1: Expr[Evolution[A]], as2: Expr[Evolution[A]])
      extends Expr[Evolution[A]](List(as1, as2))

  final case class ZipWith[A, B, C](fa: Expr[Evolution[A]], fb: Expr[Evolution[B]], f: Expr[A => B => C])
      extends Expr[Evolution[C]](List(fa, fb, f))
  final case class Take[A](fa: Expr[Evolution[A]], n: Expr[Int]) extends Expr[Evolution[A]](List(n, fa))
  final case class TakeWhile[A](fa: Expr[Evolution[A]], predicate: Expr[A => Boolean])
      extends Expr[Evolution[A]](List(fa, predicate))
  final case class Map[A, B](fa: Expr[Evolution[A]], f: Expr[A => B]) extends Expr[Evolution[B]](List(fa, f))
  final case class SlidingMap[A, B](fa: Expr[Evolution[A]], f: Expr[A => A => B])
      extends Expr[Evolution[B]](List(fa, f))
  final case class MapWithDerivative[A, B](
    fa: Expr[Evolution[A]],
    f: Expr[A => A => B],
    sg: Additive[A, A, A],
    inv: Invertible[A]
  ) extends Expr[Evolution[B]](List(fa, f))

  final case class Iterate[A](f: Expr[A => A], start: Expr[A]) extends Expr[Evolution[A]](List(f, start))
  final case class Iterate2[A](f: Expr[A => A => A], a0: Expr[A], a1: Expr[A])
      extends Expr[Evolution[A]](List(f, a0, a1))

  final case class Roll[A](f: Expr[Evolution[A => A]], start: Expr[A]) extends Expr[Evolution[A]](List(f, start))
  final case class Roll2[A](f: Expr[Evolution[A => A => A]], a0: Expr[A], a1: Expr[A])
      extends Expr[Evolution[A]](List(f, a0, a1))

  final case class FlatMap[A, B](fa: Expr[Evolution[A]], f: Expr[A => Evolution[B]])
      extends Expr[Evolution[B]](List(fa, f))
  final case class Flatten[A, B](ffa: Expr[Evolution[Evolution[A]]]) extends Expr[Evolution[B]](List(ffa))
  final case class Parallel[A](ffa: Expr[Evolution[Evolution[A]]]) extends Expr[Evolution[A]](List(ffa))
  final case class Integrate[A](start: Expr[A], speed: Expr[Evolution[A]], semigroup: Additive[A, A, A])
      extends Expr[Evolution[A]](List(start, speed))
  final case class Solve1[A](speed: Expr[Evolution[A => A]], start: Expr[A], semigroup: Additive[A, A, A])
      extends Expr[Evolution[A]](List(speed, start))
  final case class Solve2[A](
    acc: Expr[Evolution[A => A => A]],
    a0: Expr[A],
    v0: Expr[A],
    semigroup: Additive[A, A, A]
  ) extends Expr[Evolution[A]](List(acc, a0, v0))
  final case class WithFirst[A, B](as: Expr[Evolution[A]], f: Expr[A => Evolution[B]])
      extends Expr[Evolution[B]](List(as, f))

  // Distributions
  final case class Constant[T](t: Expr[T]) extends Expr[Evolution[T]](List(t))
  final case class Range(from: Expr[Double], to: Expr[Double], step: Expr[Double])
      extends Expr[Evolution[Double]](List(from, to, step))
  final case class Uniform(from: Expr[Double], to: Expr[Double]) extends Expr[Evolution[Double]](List(from, to))
  final case class UniformChoice[T](choices: Expr[List[T]]) extends Expr[Evolution[T]](List(choices))
  final case class UniformDiscrete(from: Expr[Double], to: Expr[Double], step: Expr[Double])
      extends Expr[Evolution[Double]](List(from, to, step))
  final case class UniformFrom[T](n: Expr[Int], ft: Expr[Evolution[T]]) extends Expr[Evolution[T]](List(n, ft))
  final case class Normal(μ: Expr[Double], σ: Expr[Double]) extends Expr[Evolution[Double]](List(μ, σ))
  final case class Noise() extends Expr[Evolution[Point => Double]](Nil)
  final case class OctaveNoise() extends Expr[Evolution[Int => Double => Point => Double]](Nil)

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
  }
}
