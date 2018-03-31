package evolution.drawing.algebra.interpreter

import cats.kernel.Group
import evolution.drawing.algebra.evo.interpreter.{Builder => EvoBuilder}
import evolution.drawing.algebra.{DrawingAlgebra, Type}
import Type._
import evolution.drawing.algebra.evo.{EvoAlgebra, EvoExpr}
import evolution.drawing.algebra.interpreter.ToEvo.Repr
import cats.syntax.all._
import evolution.geometry.Point

object ToEvo {
  type Repr[E, A] = E => EvoExpr[Long, A]
}

class ToEvo extends DrawingAlgebra[Repr] {
  private val builder = new EvoBuilder[Long]
  override def const[E, T: Type](x: T): Repr[E, T] =
    _ => builder.constant(x)
  override def mul[E, T: Type](k: Repr[E, Double], t: Repr[E, T]): Repr[E, T] = {
    Type[T].foldF[Repr[E, ?], Repr[E, ?]](t)(
      doubleRepr => e => builder.zipWith(k(e), doubleRepr(e)) { (double1, double2) => double1 * double2 },
      pointRepr => e => builder.zipWith(k(e), pointRepr(e)) { (d, point) => point * d }
    )
  }
  override def add[E, T: Type](a: Repr[E, T], b: Repr[E, T]): Repr[E, T] = {
    e => builder.zipWith(a(e), b(e)) { (t1, t2) => t1 |+| t2 }
  }
  override def inverse[E, T: Type](a: Repr[E, T]): Repr[E, T] = {
    val group = Type.group[T]
    e => builder.map(a(e))(t => group.inverse(t))
  }
  override def rnd[E](from: Repr[E, Double], to: Repr[E, Double]): Repr[E, Double] = ???
  override def point[E](x: Repr[E, Double], y: Repr[E, Double]): Repr[E, Point] = ???
  override def polar[E](r: Repr[E, Double], w: Repr[E, Double]): Repr[E, Point] = ???
  override def integrate[E, T: Type](start: T, f: Repr[E, T]): Repr[E, T] = ???
  override def derive[E, T: Type](f: Repr[E, T]): Repr[E, T] = ???
  override def slowDown[E, T: Type](by: Repr[E, Double], drawing: Repr[E, T]): Repr[E, T] = ???
  override def choose[E, T: Type](dist: Repr[E, Double], drawing1: Repr[E, T], drawing2: Repr[E, T]): Repr[E, T] = ???
  override def dist[E](probability: Repr[E, Double], length1: Repr[E, Double], length2: Repr[E, Double]): Repr[E, Double] = ???
  override def var0[E, A]: Repr[(Repr[E, A], E), A] = ???
  override def shift[E, A, B](expr: Repr[E, A]): Repr[(Repr[E, B], E), A] = ???
  override def let[E, A, B](name: String, value: Repr[E, A])(expr: Repr[(Repr[E, A], E), B]): Repr[E, B] = ???
}
