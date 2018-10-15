package evolution.primitive.algebra.binding
import cats.~>

trait Binding[R[_], VarName] {
  def varName(name: String): VarName
  def var0[A]: R[A]
  def shift[A](expr: R[A]): R[A]
  def let[A, B](name: VarName, value: R[A])(expr: R[B]): R[B]
  def lambda[A, B](name: VarName, expr: R[B]): R[A => B]
  def app[A, B](f: R[A => B], a: R[A]): R[B]
  def fix[A](expr: R[A => A]): R[A]
}

class MappedBinding[R1[_], R2[_], VarName](alg: Binding[R1, VarName], to: R1 ~> R2, from: R2 ~> R1)
    extends Binding[R2, VarName] {
  def varName(name: String): VarName =
    alg.varName(name)
  def var0[A]: R2[A] =
    to(alg.var0)
  def shift[A](expr: R2[A]): R2[A] =
    to(alg.shift(from(expr)))
  def let[A, B](name: VarName, value: R2[A])(expr: R2[B]): R2[B] =
    to(alg.let(name, from(value))(from(expr)))
  def lambda[A, B](name: VarName, expr: R2[B]): R2[A => B] =
    to(alg.lambda(name, from(expr)))
  def app[A, B](f: R2[A => B], a: R2[A]): R2[B] =
    to(alg.app(from(f), from(a)))
  def fix[A](expr: R2[A => A]): R2[A] =
    to(alg.fix(from(expr)))
}
