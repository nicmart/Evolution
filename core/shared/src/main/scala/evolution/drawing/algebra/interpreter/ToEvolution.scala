package evolution.drawing.algebra.interpreter

import cats.kernel.Group
import cats.implicits._
import evolution.algebra
import evolution.algebra.Evolution
import evolution.drawing.algebra.{DrawingAlgebra, Type}
import evolution.geometry.Point
import evolution.algebra.syntax.all._

object ToEvolution extends DrawingAlgebra[CtxEvolution] {
  override def const[T: Type](x: T): List[Any] => StaticOrDynamicEvolution[T] =
    _ => Static(x)

  override def mul[T: Type](k: CtxEvolution[Double], t: CtxEvolution[T]): CtxEvolution[T] = {
    implicit val group: Group[T] = Type.group[T]
    implicit val groupDouble: Group[Double] = Type.group[Double]
    Type[T].foldF[CtxEvolution[?], CtxEvolution[?]](t)(mulDouble(k), mulPoint(k))
  }

  private def mulDouble(k: CtxEvolution[Double])(t: CtxEvolution[Double]): CtxEvolution[Double] =
    e =>
      (k(e), t(e)) match {
        case (Static(kVal), Static(tVal)) => Static(kVal * tVal)
        case (Static(kVal), Dynamic(tEvo)) =>
          Dynamic(new Evolution[Double] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] = tEvo.run.map(kVal * _)
          })
        case (Dynamic(kEvo), Static(tVal)) =>
          Dynamic(new Evolution[Double] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] = kEvo.run.map(_ * tVal)
          })
        case (Dynamic(kEvo), Dynamic(tEvo)) =>
          Dynamic(new Evolution[Double] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
              kEvo.run.zipWith(tEvo.run)(_ * _)
          })
    }

  private def mulPoint(k: CtxEvolution[Double])(t: CtxEvolution[Point]): CtxEvolution[Point] =
    e =>
      (k(e), t(e)) match {
        case (Static(kVal), Static(tVal)) => Static(tVal * kVal)
        case (Static(kVal), Dynamic(tEvo)) =>
          Dynamic(new Evolution[Point] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = tEvo.run.map(_ * kVal)
          })
        case (Dynamic(kEvo), Static(tVal)) =>
          Dynamic(new Evolution[Point] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] = kEvo.run.map(tVal * _)
          })
        case (Dynamic(kEvo), Dynamic(tEvo)) =>
          Dynamic(new Evolution[Point] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
              tEvo.run.zipWith(kEvo.run)(_ * _)
          })
    }

  override def inverse[T: Type](a: CtxEvolution[T]): CtxEvolution[T] = { e =>
    implicit val group: Group[T] = Type.group[T]
    a(e) match {
      case Static(aVal) => Static(aVal.inverse)
      case Dynamic(aEvo) =>
        Dynamic(new Evolution[T] {
          override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] = aEvo.run.map(_.inverse)
        })
    }
  }

  override def add[T: Type](a: CtxEvolution[T], b: CtxEvolution[T]): CtxEvolution[T] = {
    implicit val group: Group[T] = Type.group[T]
    e =>
      (a(e), b(e)) match {
        case (Static(aVal), Static(bVal)) => Static(aVal |+| bVal)
        case (Static(aVal), Dynamic(bEvo)) =>
          Dynamic(new Evolution[T] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] = bEvo.run.map(aVal |+| _)
          })
        case (Dynamic(aEvo), Static(bVal)) =>
          Dynamic(new Evolution[T] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] = aEvo.run.map(_ |+| bVal)
          })
        case (Dynamic(aEvo), Dynamic(bEvo)) =>
          Dynamic(new Evolution[T] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
              aEvo.run.zipWith(bEvo.run)(_ |+| _)
          })
      }
  }

  /**
    * Optimise the generated evolution when both from and to are static values
    */
  override def rnd(from: CtxEvolution[Double], to: CtxEvolution[Double]): CtxEvolution[Double] =
    e =>
      (from(e), to(e)) match {
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

  override def point(x: CtxEvolution[Double], y: CtxEvolution[Double]): CtxEvolution[Point] =
    e =>
      (x(e), y(e)) match {
        case (Static(xValue), Static(yValue)) =>
          Static(Point(xValue, yValue))
        case _ =>
          Dynamic(new Evolution[Point] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
              alg.cartesian(x(e).evolution.run, y(e).evolution.run)
          })
    }
  override def polar(r: CtxEvolution[Double], w: CtxEvolution[Double]): CtxEvolution[Point] =
    e =>
      Dynamic(new Evolution[Point] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
          alg.polar(r(e).evolution.run, w(e).evolution.run)
      })
  override def integrate[T: Type](start: T, f: CtxEvolution[T]): CtxEvolution[T] =
    e =>
      Dynamic(new Evolution[T] {
        implicit val group: Group[T] = Type.group[T]
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
          alg.solveIndependent(start)(f(e).evolution.run).positional
      })
  override def derive[T: Type](f: CtxEvolution[T]): CtxEvolution[T] =
    e =>
      Dynamic(new Evolution[T] {
        implicit val group: Group[T] = Type.group[T]
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
          alg.toPhaseSpace(f(e).evolution.run).map(_._2)
      })

  // TODO Deal with failures of Ctx and asInstanceOf
  override def var0[A]: CtxEvolution[A] = { ctx =>
    ctx.head.asInstanceOf[StaticOrDynamicEvolution[A]]
  }

  override def shift[A](expr: CtxEvolution[A]): CtxEvolution[A] = {
    case _ :: tail => expr(tail)
  }

  override def let[In, Out](name: String, value: CtxEvolution[In])(expr: CtxEvolution[Out]): CtxEvolution[Out] =
    ctx => expr(value(ctx) :: ctx)

  override def slowDown[T: Type](by: CtxEvolution[Double], drawing: CtxEvolution[T]): CtxEvolution[T] =
    ctx =>
      (by(ctx), drawing(ctx)) match {
        case (_, Static(drawingVal)) => Static(drawingVal)
        case (Static(byVal), Dynamic(drawingEvo)) =>
          Dynamic(new Evolution[T] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
              alg.slowDown(drawingEvo.run, byVal.toInt)
          })
        case (Dynamic(byEvo), Dynamic(drawingEvo)) =>
          Dynamic(new Evolution[T] {
            override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
              alg.slowDownBy(drawingEvo.run, alg.map(byEvo.run)(_.toInt))
          })
    }

  override def choose[T: Type](
    p: CtxEvolution[Double],
    drawing1: CtxEvolution[T],
    drawing2: CtxEvolution[T]
  ): CtxEvolution[T] = { ctx =>
    Dynamic(new Evolution[T] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
        alg.chooseBy(p(ctx).evolution.run.map(_ < .5), drawing1(ctx).evolution.run, drawing2(ctx).evolution.run)
    })
  }

  override def dist(
    p: CtxEvolution[Double],
    length1: CtxEvolution[Double],
    length2: CtxEvolution[Double]
  ): CtxEvolution[Double] =
    ctx =>
      Dynamic(new Evolution[Double] {
        override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
          alg.dist(p(ctx).evolution.run, length1(ctx).evolution.run, length2(ctx).evolution.run)
      })
}
