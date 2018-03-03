package evolution.drawing.algebra.interpreter

import cats.data.NonEmptyList
import cats.kernel.Group
import cats.syntax.group._
import evolution.algebra
import evolution.algebra.Evolution
import evolution.drawing.algebra.{DrawingAlgebra, Type}
import evolution.geometry.Point
import evolution.algebra.syntax.all._

object ToEvolution extends DrawingAlgebra[CtxEvolution] {
  override def const[E, T: Type](x: T): E => StaticOrDynamicEvolution[T] =
    _ => Static(x)

  override def mul[E, T: Type](k: CtxEvolution[E, Double], t: CtxEvolution[E, T]): CtxEvolution[E, T] = {
    implicit val group: Group[T] = Type.group[T]
    implicit val groupDouble: Group[Double] = Type.group[Double]
    Type[T].foldF[CtxEvolution[E, ?], CtxEvolution[E, ?]](t)(
      mulDouble[E](k),
      mulPoint[E](k)
    )
  }

  private def mulDouble[E](k: CtxEvolution[E, Double])(t: CtxEvolution[E, Double]): CtxEvolution[E, Double] =
    e => (k(e), t(e)) match {
      case (Static(kVal), Static(tVal)) => Static(kVal * tVal)
      case (Static(kVal), Dynamic(tEvo)) => Dynamic(new Evolution[Double] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] = tEvo.run.map(kVal * _)
      })
      case (Dynamic(kEvo), Static(tVal)) => Dynamic(new Evolution[Double] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] = kEvo.run.map(_ * tVal)
      })
      case (Dynamic(kEvo), Dynamic(tEvo)) => Dynamic(new Evolution[Double] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
          kEvo.run.zipWith(tEvo.run)(_ * _)
      })
    }

  private def mulPoint[E](k: CtxEvolution[E, Double])(t: CtxEvolution[E, Point]): CtxEvolution[E, Point] =
    e => (k(e), t(e)) match {
      case (Static(kVal), Static(tVal)) => Static(tVal * kVal)
      case (Static(kVal), Dynamic(tEvo)) => Dynamic(new Evolution[Point] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = tEvo.run.map(_ * kVal)
      })
      case (Dynamic(kEvo), Static(tVal)) => Dynamic(new Evolution[Point] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = kEvo.run.map(tVal * _)
      })
      case (Dynamic(kEvo), Dynamic(tEvo)) => Dynamic(new Evolution[Point] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
          tEvo.run.zipWith(kEvo.run)(_ * _)
      })
    }

  override def inverse[E, T: Type](a: CtxEvolution[E, T]): CtxEvolution[E, T] = {
    implicit val group: Group[T] = Type.group[T]
    e => a(e) match {
      case Static(aVal) => Static(aVal.inverse)
      case Dynamic(aEvo) => Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] = aEvo.run.map(_.inverse)
      })
    }
  }

  override def add[E, T: Type](a: CtxEvolution[E, T], b: CtxEvolution[E, T]): CtxEvolution[E, T] = {
    implicit val group: Group[T] = Type.group[T]
    e => (a(e), b(e)) match {
      case (Static(aVal), Static(bVal)) => Static(aVal |+| bVal)
      case (Static(aVal), Dynamic(bEvo)) => Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] = bEvo.run.map(aVal |+| _)
      })
      case (Dynamic(aEvo), Static(bVal)) => Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] = aEvo.run.map(_ |+| bVal)
      })
      case (Dynamic(aEvo), Dynamic(bEvo)) => Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
          aEvo.run.zipWith(bEvo.run)(_ |+| _)
      })
    }
  }

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

  override def slowDown[E, T: Type](by: CtxEvolution[E, Double], drawing: CtxEvolution[E, T]): CtxEvolution[E, T] =
    ctx => (by(ctx), drawing(ctx)) match {
      case (_, Static(drawingVal)) => Static(drawingVal)
      case (Static(byVal), Dynamic(drawingEvo)) => Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
          alg.slowDown(drawingEvo.run, byVal.toInt)
      })
      case (Dynamic(byEvo), Dynamic(drawingEvo)) => Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
          alg.slowDownBy(drawingEvo.run, alg.map(byEvo.run)(_.toInt))
      })
    }

  override def choose[E, T: Type](p: CtxEvolution[E, Double], drawing1: CtxEvolution[E, T], drawing2: CtxEvolution[E, T]): CtxEvolution[E, T] = {
    ctx =>
      Dynamic(new Evolution[T] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
          alg.chooseBy(
            p(ctx).evolution.run.map(_ < .5),
            drawing1(ctx).evolution.run,
            drawing2(ctx).evolution.run
          )
      })
  }

  override def dist[E](p: CtxEvolution[E, Double], length1: CtxEvolution[E, Double], length2: CtxEvolution[E, Double]): CtxEvolution[E, Double] =
    ctx =>
      Dynamic(new Evolution[Double] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
          alg.dist(
            p(ctx).evolution.run,
            length1(ctx).evolution.run,
            length2(ctx).evolution.run
          )
      })
}
