package evolution.primitive.algebra.chain.interpreter
import evolution.primitive.algebra.{ConstString, CtxString}
import evolution.primitive.algebra.chain.Chain

object ChainSerializer extends Chain[ConstString, ConstString, CtxString] {
  override def empty[A]: CtxString[A] =
    _ => "empty"
  override def cons[A](head: CtxString[A], tail: CtxString[A]): CtxString[A] =
    ctx => s"cons(${head(ctx)}, ${tail(ctx)})"
  override def mapEmpty[A](eva: CtxString[A])(eva2: CtxString[A]): CtxString[A] =
    ctx => s"mapEmpty(${eva(ctx)}, ${eva2(ctx)})"
  override def mapCons[A, B](eva: CtxString[A])(f: CtxString[String => String => String]): CtxString[B] =
    ctx => s"mapCons(${eva(ctx)}, ${f(ctx)})"
}
