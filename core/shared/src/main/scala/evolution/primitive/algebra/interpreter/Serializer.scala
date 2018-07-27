package evolution.primitive.algebra.interpreter

import evolution.geometry.Point
import evolution.primitive.algebra._

object Serializer extends DrawingAlgebra[CtxString, CtxString] {
  override val drawing: CoreDrawingAlgebra[CtxString, CtxString] = new CoreDrawingAlgebra[CtxString, CtxString] {
    override def empty[A]: CtxString[A] =
      _ => "empty"
    override def cons[A](head: CtxString[A], tail: CtxString[A]): CtxString[A] =
      ctx => s"cons(${head(ctx)}, ${tail(ctx)})"
    override def mapEmpty[A](eva: CtxString[A])(eva2: CtxString[A]): CtxString[A] =
      ctx => s"mapEmpty(${eva(ctx)})(${eva2(ctx)})"
    override def mapCons[A, B](eva: CtxString[A])(f: CtxString[B]): CtxString[B] =
      ctx => s"mapCons(${eva(ctx)})(x -> ${f("$x" :: ctx)})"
  }
  override val scalar: ScalarAlgebra[CtxString] = new ScalarAlgebra[CtxString] {
    override def double(d: Double): CtxString[Double] = _ => d.toString
    override def point(p: Point): CtxString[Point] = _ => p.toString
  }
  override val bindS: BindingAlgebra[CtxString] = BindingAlgebraSerializer
  override val bindF: BindingAlgebra[CtxString] = BindingAlgebraSerializer
}
