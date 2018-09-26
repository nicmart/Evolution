package evolution.primitive.algebra.evolution.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.interpreter.{BindingAlgebraSerializer, ConstString, CtxString}
import evolution.primitive.algebra.BindingAlgebra
import evolution.primitive.algebra.list.ListAlgebra

object EvolutionAlgebraSerializer extends EvolutionAlgebra[ConstString, ConstString, CtxString, String] {
  override val drawing: ListAlgebra[ConstString, ConstString, CtxString] =
    new ListAlgebra[ConstString, ConstString, CtxString] {
      override def empty[A]: CtxString[A] =
        _ => "empty"
      override def cons[A](head: CtxString[A], tail: CtxString[A]): CtxString[A] =
        ctx => s"cons(${head(ctx)}, ${tail(ctx)})"
      override def mapEmpty[A](eva: CtxString[A])(eva2: CtxString[A]): CtxString[A] =
        ctx => s"mapEmpty(${eva(ctx)})(${eva2(ctx)})"
      override def mapCons[A, B](eva: CtxString[A])(f: CtxString[String => String => String]): CtxString[B] =
        ctx => s"mapCons(${eva(ctx)})(h -> t -> ${f("h" :: "t" :: ctx)})"
    }
  override val scalar: ConstantsAlgebra[RS] = new ConstantsAlgebra[RS] {
    override def double(d: Double): CtxString[Double] = _ => d.toString
    override def point(x: Double, y: Double): CtxString[Point] = _ => s"point($x, $y)"
    override def add[T: Semigroup](a: CtxString[T], b: CtxString[T]): CtxString[T] = ctx => s"add(${a(ctx)}, ${b(ctx)})"
  }
  override val bind: BindingAlgebra[CtxString, String] = BindingAlgebraSerializer
}
