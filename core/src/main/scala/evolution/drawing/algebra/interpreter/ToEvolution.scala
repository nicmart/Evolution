package evolution.drawing.algebra.interpreter

import cats.kernel.Group
import evolution.algebra
import evolution.algebra.Evolution
import evolution.drawing.algebra.{DrawingAlgebra, Type}
import evolution.geometry.Point
import evolution.algebra.syntax.all._

object ToEvolution extends DrawingAlgebra[CtxEvolution] {
  override def const[E, T: Type](x: T): E => Evolution[T] =
    _ => new Evolution[T] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
        alg.constant(x)
    }
  override def rnd[E](from: E => Evolution[Double], to: E => Evolution[Double]): E => Evolution[Double] =
    e => new Evolution[Double] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
        alg.doubleBetweenEvo(from(e).run, to(e).run)
    }
  override def point[E](x: E => Evolution[Double], y: E => Evolution[Double]): E => Evolution[Point] =
    e => new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
        alg.cartesian(x(e).run, y(e).run)
    }
  override def polar[E](r: E => Evolution[Double], w: E => Evolution[Double]): E => Evolution[Point] =
    e => new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
        alg.polar(r(e).run, w(e).run)
    }
  override def integrate[E, T: Type](start: T, f: E => Evolution[T]): E => Evolution[T] =
    e => new Evolution[T] {
      implicit val group: Group[T] = Type.group[T]
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
        alg.solveIndependent(start)(f(e).run).positional
    }
  override def derive[E, T: Type](f: E => Evolution[T]): E => Evolution[T] =
    e => new Evolution[T] {
      implicit val group: Group[T] = Type.group[T]
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
        alg.toPhaseSpace(f(e).run).map(_._2)
    }
  override def var0[E, A]: CtxEvolution[(CtxEvolution[E, A], E), A] = {
    case (ctx, e) => ctx(e)
  }

  override def shift[E, A, B](expr: CtxEvolution[E, A]): CtxEvolution[(CtxEvolution[E, B], E), A] = {
    case (_, e) => expr(e)
  }

  override def let[E, In, Out](name: String, value: CtxEvolution[E, In])
  (expr: CtxEvolution[(CtxEvolution[E, In], E), Out]): E => Evolution[Out] =
  ctx => expr((value, ctx))
}
