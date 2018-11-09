package evolution.primitive.algebra

import _root_.evolution.geometry.Point
import binding.{Binding => BindingAlg}
import evolution._
import chain._
import constants._
import cats.kernel.Semigroup

trait TestInterpreters {

  sealed trait Binding[A]
  case class Lift[A](get: A) extends Binding[A]
  case class Var0[A]() extends Binding[A]
  case class Shift[A](expr: Binding[A]) extends Binding[A]
  case class Let[A, B](name: String, value: Binding[A], expr: Binding[B]) extends Binding[B]
  case class Lambda[A, B](name: String, expr: Binding[B]) extends Binding[A => B]
  case class App[A, B](f: Binding[A => B], a: Binding[A]) extends Binding[B]
  case class Fix[A](expr: Binding[A => A]) extends Binding[A]

  case class Value[T](value: T) extends Binding[T]
  case class PointConstant(x: Binding[Double], y: Binding[Double]) extends Binding[Point]
  case class Add[T: Semigroup](a: Binding[T], b: Binding[T]) extends Binding[T]

  sealed trait ListExpr[A]
  case class Empty[A]() extends ListExpr[A]
  case class Cons[A](head: Binding[A], tail: Binding[ListExpr[A]]) extends ListExpr[A]
  case class MapEmpty[A](eva: Binding[ListExpr[A]], eva2: Binding[ListExpr[A]]) extends ListExpr[A]
  case class MapCons[A, B](eva: Binding[ListExpr[A]], f: Binding[A => ListExpr[A] => ListExpr[B]]) extends ListExpr[B]

  implicit def liftToBinding[T](t: T): Binding[T] = Lift(t)
  implicit def liftListExprToBinding[T](t: ListExpr[T]): Binding[ListExpr[T]] = Lift(t)
  def unlift[T](binding: Binding[T]): T = binding match {
    case Lift(t) => t
  }

  object BindingTestInterpreter extends BindingAlg[Binding, String, String] {
    override def v(name: String): String = name
    override def var0[A]: Binding[A] = Var0[A]()
    override def shift[A](expr: Binding[A]): Binding[A] = Shift(expr)
    override def let[A, B](name: String, value: Binding[A], expr: Binding[B]): Binding[B] = Let(name, value, expr)
    override def lambda[A, B](name: String, expr: Binding[B]): Binding[A => B] = Lambda(name, expr)
    override def app[A, B](f: Binding[A => B], a: Binding[A]): Binding[B] = App(f, a)
    override def fix[A](expr: Binding[A => A]): Binding[A] = Fix(expr)
  }

  object ConstantsTestInterpreter extends Constants[Binding, Double] {
    override def double(d: Double): Binding[Double] = d
    override def point(x: Binding[Double], y: Binding[Double]): Binding[Point] =
      PointConstant(x, y)
    override def add[T: Semigroup](a: Binding[T], b: Binding[T]): Binding[T] = Add(a, b)
  }

  object ChainTestInterpreter extends Chain[ListExpr, Binding] {
    override def empty[A]: Binding[ListExpr[A]] = Empty[A]()
    override def cons[A](head: Binding[A], tail: Binding[ListExpr[A]]): Binding[ListExpr[A]] =
      Cons(head, tail)
    override def mapEmpty[A](eva: Binding[ListExpr[A]], eva2: Binding[ListExpr[A]]): Binding[ListExpr[A]] =
      MapEmpty(eva, eva2)
    override def mapCons[A, B](
      eva: Binding[ListExpr[A]]
    )(f: Binding[A => ListExpr[A] => ListExpr[B]]): Binding[ListExpr[B]] =
      MapCons(eva, f)
  }

  object EvolutionAlgebraTestInterpreter extends Evolution[ListExpr, Binding, Double, String, String] {
    override val chain: Chain[ListExpr, Binding] = ChainTestInterpreter
    override val constants: Constants[Binding, Double] = ConstantsTestInterpreter
    override val bind: BindingAlg[Binding, String, String] = BindingTestInterpreter
  }
}
