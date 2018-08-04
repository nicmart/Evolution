package evolution.drawing.algebra.interpreter

import cats.data.NonEmptyList
import evolution.drawing.algebra._

object Serializer extends DrawingAlgebra[CtxString] {
  override def const[T: Type](x: T): List[String] => String =
    e => Type[T].fold[λ[X => String]](x)(_.toString, point => s"point(${point.x}, ${point.y})")
  override def mul[T: Type](k: CtxString[Double], t: CtxString[T]): List[String] => String =
    e => s"mul(${k(e)}, ${t(e)})"
  override def add[T: Type](a: CtxString[T], b: CtxString[T]): List[String] => String =
    e => s"add(${a(e)}, ${b(e)})"
  override def inverse[T: Type](a: CtxString[T]): CtxString[T] =
    e => s"inverse(${a(e)})"
  override def rnd(from: List[String] => String, to: List[String] => String): List[String] => String =
    e => s"rnd(${from(e)}, ${to(e)})"
  override def point(x: List[String] => String, y: List[String] => String): List[String] => String =
    e => s"point(${x(e)}, ${y(e)})"
  override def polar(r: List[String] => String, w: List[String] => String): List[String] => String =
    e => s"polar(${r(e)}, ${w(e)})"
  override def integrate[T: Type](start: T, f: List[String] => String): List[String] => String =
    e => s"integrate(${const(start).apply(e)}, ${f(e)})"
  override def derive[T: Type](f: List[String] => String): List[String] => String =
    e => s"derive(${f(e)})"
  override def var0[A]: List[String] => String =
    ctx => "$" + ctx.headOption.getOrElse("x")
  override def shift[A](expr: CtxString[A]): List[String] => String =
    ctx => expr(ctx.tail)
  override def let[A, B](name: String, value: CtxString[A])(expr: CtxString[B]): CtxString[B] =
    ctx => s"$name = ${value(ctx)}\n${expr(name :: ctx)}"
  override def slowDown[T: Type](by: CtxString[Double], drawing: CtxString[T]): CtxString[T] =
    ctx => s"slowDown(${by(ctx)}, ${drawing(ctx)})"
  override def choose[T: Type](p: CtxString[Double], drawing1: CtxString[T], drawing2: CtxString[T]): CtxString[T] =
    ctx => s"choose(${p(ctx)}, ${drawing1(ctx)}, ${drawing2(ctx)})"
  override def dist(
    probability: CtxString[Double],
    length1: CtxString[Double],
    length2: CtxString[Double]
  ): CtxString[Double] =
    ctx => s"dist(${probability(ctx)}, ${length1(ctx)}, ${length2(ctx)})"
}
