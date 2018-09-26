package evolution.primitive.algebra.constants.interpreter
import cats.kernel.Semigroup
import evolution.geometry.Point
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.evolution.interpreter.EvolutionAlgebraSerializer.RS

object ConstantsAlgebraSerializer extends ConstantsAlgebra[RS] {
  override def double(d: Double): CtxString[Double] = _ => d.toString
  override def point(x: Double, y: Double): CtxString[Point] = _ => s"point($x, $y)"
  override def add[T: Semigroup](a: CtxString[T], b: CtxString[T]): CtxString[T] = ctx => s"add(${a(ctx)}, ${b(ctx)})"
}
