package evolution.drawing.algebra.interpreter

import cats.kernel.Group
import evolution.algebra
import evolution.algebra.Evolution
import evolution.drawing.algebra.DrawingAlgebra
import evolution.geometry.Point
import evolution.algebra.syntax.all._

object ToEvolution extends DrawingAlgebra[Evolution] {
  override def const[T: DrawingAlgebra.Type](x: T): Evolution[T] = new Evolution[T] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
      alg.pure(x)
  }
  override def rnd(from: Double, to: Double): Evolution[Double] = new Evolution[Double] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Double] =
      alg.doubleBetween(from, to)
  }
  override def cartesian(x: Evolution[Double], y: Evolution[Double]): Evolution[Point] = new Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
      alg.cartesian(x.run, y.run)
  }
  override def polar(r: Evolution[Double], w: Evolution[Double]): Evolution[Point] = new Evolution[Point] {
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[Point] =
      alg.polar(r.run, w.run)
  }
  override def integrate[T: DrawingAlgebra.Type](start: T, f: Evolution[T]): Evolution[T] = new Evolution[T] {
    implicit val group: Group[T] = DrawingAlgebra.typeInstance[T].group
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
      alg.solveIndependent(start)(f.run).positional
  }
  override def derive[T: DrawingAlgebra.Type](f: Evolution[T]): Evolution[T] = new Evolution[T] {
    implicit val group: Group[T] = DrawingAlgebra.typeInstance[T].group
    override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[T] =
      alg.toPhaseSpace(f.run).map(_._2)
  }
}
