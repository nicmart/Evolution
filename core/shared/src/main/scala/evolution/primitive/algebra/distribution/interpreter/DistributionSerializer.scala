package evolution.primitive.algebra.distribution.interpreter
import evolution.primitive.algebra.CtxString
import evolution.primitive.algebra.distribution.Distribution

class DistributionSerializer[F[_]] extends Distribution[F, CtxString] {
  override def uniform(from: CtxString[Double], to: CtxString[Double]): CtxString[F[Double]] =
    ctx => s"uniform(${from(ctx)}, ${to(ctx)})"
  override def uniformChoice[T](ts: List[CtxString[T]]): CtxString[F[T]] =
    ctx => s"uniformChoice(${ts.map(_(ctx)).mkString(", ")})"
}
