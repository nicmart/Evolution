package evolution.drawing.algebra.interpreter

import cats.kernel.Group
import evolution.algebra
import evolution.algebra.Evolution
import evolution.drawing.algebra.{DrawingAlgebra, Type}
import evolution.geometry.Point
import evolution.algebra.syntax.all._

object ToEvolution extends DrawingAlgebra[CtxEvolution] {
  override def const[E, T: Type](x: T): E => StaticOrDynamicEvolution[T] =
    _ => Static(x)

  /**
    * Optimise the generated evolution when both from and to are static values
    */
  override def rnd[E](from: E => StaticOrDynamicEvolution[Double], to: E => StaticOrDynamicEvolution[Double]): E => StaticOrDynamicEvolution[Double] =
    e => (from(e), to(e)) match {
      case (Static(fromValue), Static(toValue)) =>
        Dynamic(new Evolution[Double] {
          override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
            alg.doubleBetween(fromValue, toValue)
        })
      case _ =>
        Dynamic(new Evolution[Double] {
          override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
            alg.doubleBetweenEvo(from(e).evolution.run, to(e).evolution.run)
        })
    }

  override def point[E](x: E => StaticOrDynamicEvolution[Double], y: E => StaticOrDynamicEvolution[Double]): E => StaticOrDynamicEvolution[Point] =
    e => (x(e), y(e)) match {
      case (Static(xValue), Static(yValue)) =>
        Static(Point(xValue, yValue))
      case _ =>
        Dynamic(new Evolution[Point] {
          override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
            alg.cartesian(x(e).evolution.run, y(e).evolution.run)
        })
  }
  override def polar[E](r: E => StaticOrDynamicEvolution[Double], w: E => StaticOrDynamicEvolution[Double]): E => StaticOrDynamicEvolution[Point] =
    e => Dynamic(new Evolution[Point] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
        alg.polar(r(e).evolution.run, w(e).evolution.run)
    })
  override def integrate[E, T: Type](start: T, f: E => StaticOrDynamicEvolution[T]): E => StaticOrDynamicEvolution[T] =
    e => Dynamic(new Evolution[T] {
      implicit val group: Group[T] = Type.group[T]
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
        alg.solveIndependent(start)(f(e).evolution.run).positional
    })
  override def derive[E, T: Type](f: E => StaticOrDynamicEvolution[T]): E => StaticOrDynamicEvolution[T] =
    e => Dynamic(new Evolution[T] {
      implicit val group: Group[T] = Type.group[T]
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
        alg.toPhaseSpace(f(e).evolution.run).map(_._2)
    })
  override def var0[E, A]: CtxEvolution[(CtxEvolution[E, A], E), A] = {
    case (ctx, e) => ctx(e)
  }

  override def shift[E, A, B](expr: CtxEvolution[E, A]): CtxEvolution[(CtxEvolution[E, B], E), A] = {
    case (_, e) => expr(e)
  }

  override def let[E, In, Out](name: String, value: CtxEvolution[E, In])
  (expr: CtxEvolution[(CtxEvolution[E, In], E), Out]): E => StaticOrDynamicEvolution[Out] =
  ctx => expr((value, ctx))
}
