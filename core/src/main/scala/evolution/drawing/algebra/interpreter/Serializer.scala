package evolution.drawing.algebra.interpreter

import evolution.drawing.algebra._
import evolution.geometry.Point

object Serializer extends DrawingAlgebra[CtxString] {
  override def const[E, T: Type](x: T): List[String] => String =
    e => Type[T].fold[λ[X => String]](x)(_.toString, point => s"point(${point.x},${point.y})")
  override def mul[E, T: Type](k: CtxString[E, Double], t: CtxString[E, T]): List[String] => String =
    e => s"mul(${k(e)}, ${t(e)})"
  override def add[E, T: Type](a: CtxString[E, T], b: CtxString[E, T]): List[String] => String =
    e => s"add(${a(e)}, ${b(e)})"
  override def inverse[E, T: Type](a: CtxString[E, T]): CtxString[E, T] =
    e => s"inverse(${a(e)})"
  override def rnd[E](from: List[String] => String, to: List[String] => String): List[String] => String =
    e => s"rnd(${from(e)},${to(e)})"
  override def point[E](x: List[String] => String, y: List[String] => String): List[String] => String =
    e => s"point(${x(e)},${y(e)})"
  override def polar[E](r: List[String] => String, w: List[String] => String): List[String] => String =
    e => s"polar(${r(e)},${w(e)})"
  override def integrate[E, T: Type](start: T, f: List[String] => String): List[String] => String =
    e => s"integrate(${const(start).apply(e)},${f(e)})"
  override def derive[E, T: Type](f: List[String] => String): List[String] => String =
    e => s"derive(${f(e)})"
  override def var0[E, A]: CtxString[(CtxString[E, A], E), A] =
    ctx => "$" + ctx.headOption.getOrElse("x")
  override def shift[E, A, B](expr: CtxString[E, A]): CtxString[(CtxString[E, B], E), A] =
    ctx => expr(ctx.tail)
  override def let[E, A, B](name: String, value: CtxString[E, A])(expr: CtxString[(CtxString[E, A], E), B]): CtxString[E, B] =
    ctx => s"let($name,${value(ctx)},${expr(name :: ctx)})"
  override def slowDown[E, T: Type](by: CtxString[E, Double], drawing: CtxString[E, T]): CtxString[E, T] =
    ctx => s"slowDown(${by(ctx)}, ${drawing(ctx)})"
}
