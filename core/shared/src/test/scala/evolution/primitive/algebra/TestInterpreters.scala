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

  sealed trait Constant[A]
  case class Value[T](value: T) extends Constant[T]
  case class PointConstant(x: Binding[Constant[Double]], y: Binding[Constant[Double]]) extends Constant[Point]
  case class Add[T: Semigroup](a: Binding[Constant[T]], b: Binding[Constant[T]]) extends Constant[T]

  sealed trait ListExpr[A]
  case class Empty[A]() extends ListExpr[A]
  case class Cons[A](head: Binding[Constant[A]], tail: Binding[ListExpr[A]]) extends ListExpr[A]
  case class MapEmpty[A](eva: Binding[ListExpr[A]], eva2: Binding[ListExpr[A]]) extends ListExpr[A]
  case class MapCons[A, B](eva: Binding[ListExpr[A]], f: Binding[Constant[A] => ListExpr[A] => ListExpr[B]])
      extends ListExpr[B]

  implicit def liftToConstant[T](t: T): Constant[T] = Value(t)
  implicit def liftToBinding[T](t: T): Binding[T] = Lift(t)
  implicit def liftListExprToBinding[T](t: ListExpr[T]): Binding[ListExpr[T]] = Lift(t)
  implicit def liftConstantToBinding[T](t: Constant[T]): Binding[Constant[T]] = Lift(t)
  implicit def liftToBindingConstant[T](t: T): Binding[Constant[T]] = Lift(Value(t))
  def unlift[T](binding: Binding[T]): T = binding match {
    case Lift(t) => t
  }

  object BindingTestInterpreter extends BindingAlg[Binding, String] {
    override def varName(name: String): String = name
    override def var0[A]: Binding[A] = Var0[A]()
    override def shift[A](expr: Binding[A]): Binding[A] = Shift(expr)
    override def let[A, B](name: String, value: Binding[A])(expr: Binding[B]): Binding[B] = Let(name, value, expr)
    override def lambda[A, B](name: String, expr: Binding[B]): Binding[A => B] = Lambda(name, expr)
    override def app[A, B](f: Binding[A => B], a: Binding[A]): Binding[B] = App(f, a)
    override def fix[A](expr: Binding[A => A]): Binding[A] = Fix(expr)
  }

  object ConstantsTestInterpreter extends Constants[Composed[Binding, Constant, ?]] {
    override def double(d: Double): Binding[Constant[Double]] = d
    override def point(x: Binding[Constant[Double]], y: Binding[Constant[Double]]): Binding[Constant[Point]] =
      PointConstant(x, y)
    override def add[T: Semigroup](a: Binding[Constant[T]], b: Binding[Constant[T]]): Binding[Constant[T]] = Add(a, b)
  }

  object ChainTestInterpreter extends Chain[Constant, ListExpr, Binding] {
    override def empty[A]: Binding[ListExpr[A]] = Empty[A]()
    override def cons[A](head: Binding[Constant[A]], tail: Binding[ListExpr[A]]): Binding[ListExpr[A]] =
      Cons(head, tail)
    override def mapEmpty[A](eva: Binding[ListExpr[A]])(eva2: Binding[ListExpr[A]]): Binding[ListExpr[A]] =
      MapEmpty(eva, eva2)
    override def mapCons[A, B](
      eva: Binding[ListExpr[A]]
    )(f: Binding[Constant[A] => ListExpr[A] => ListExpr[B]]): Binding[ListExpr[B]] =
      MapCons(eva, f)
  }

  object EvolutionAlgebraTestInterpreter extends Evolution[Constant, ListExpr, Binding, String] {
    override val list: Chain[Constant, ListExpr, Binding] = ChainTestInterpreter
    override val constants: Constants[Composed[Binding, Constant, ?]] = ConstantsTestInterpreter
    override val bind: BindingAlg[Binding, String] = BindingTestInterpreter
  }
}
