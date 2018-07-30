package evolution.primitive.algebra.interpreter

import evolution.geometry.Point
import evolution.primitive.algebra._

object Serializer extends DrawingAlgebra[ConstString, ConstString, CtxString] {
  override val drawing: CoreDrawingAlgebra[RS, RF] =
    new CoreDrawingAlgebra[RS, RF] {
      override def empty[A]: CtxString[A] =
        _ => "empty"
      override def cons[A](head: CtxString[A], tail: CtxString[A]): CtxString[A] =
        ctx => s"cons(${head(ctx)}, ${tail(ctx)})"
      override def mapEmpty[A](eva: CtxString[A])(eva2: CtxString[A]): CtxString[A] =
        ctx => s"mapEmpty(${eva(ctx)})(${eva2(ctx)})"
      override def mapCons[A, B](eva: CtxString[A])(f: CtxString[B]): CtxString[B] =
        ctx => s"mapCons(${eva(ctx)})(x -> ${f("$x" :: ctx)})"
    }
  override val scalar: ScalarAlgebra[RS] = new ScalarAlgebra[RS] {
    override def double(d: Double): CtxString[Double] = _ => d.toString
    override def point(p: Point): CtxString[Point] = _ => p.toString
  }
  override val bind: BindingAlgebra[CtxString] = BindingAlgebraSerializer
}
