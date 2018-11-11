package evolution.primitive.algebra.evolution.interpreter
import Types._
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution

class EvolutionTypedSerializer extends Evolution[F, R, Double, String, String] {
  override val chain: Chain[F, R] = new Chain[F, R] {
    override def empty[A]: R[F[A]] = typeInfo => s"empty:$typeInfo"
    override def cons[A](head: R[A], tail: R[F[A]]): R[F[A]] = ???
    override def mapEmpty[A](eva: R[F[A]], eva2: R[F[A]]): R[F[A]] = ???
    override def mapCons[A, B](eva: R[F[A]])(f: R[A => F[A] => F[B]]): R[F[B]] = ???
  }

  override val constants: Constants[R, Double] = new Constants[R, Double] {
    override def double(d: Double): R[Double] = ???
    override def point(x: R[Double], y: R[Double]): R[Point] = ???
    override def add[T: Semigroup](a: R[T], b: R[T]): R[T] = ???
  }

  override val bind: Binding[R, String, String] = new Binding[R, String, String] {
    override def v(name: String): String = ???
    override def var0[A]: R[A] = ???
    override def shift[A](expr: R[A]): R[A] = ???
    override def let[A, B](variable: String, value: R[A], expr: R[B]): R[B] = ???
    override def lambda[A, B](variable: String, expr: R[B]): R[A => B] = ???
    override def app[A, B](f: R[A => B], a: R[A]): R[B] = ???
    override def fix[A](expr: R[A => A]): R[A] = ???
  }
}

object Types {
  case class TypeInfo[T](info: String) {
    override def toString: String = info
  }

  case class F[T]()

  type R[T] = TypeInfo[T] => String
}
