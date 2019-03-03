package evolution.data
import evolution.data.EvaluationContext._
import evolution.language.InterpreterModule
import evolution.materialization.{ RNG, RNGRepr }

import scala.util.Random

trait EvaluationModule[F[_]] extends ExpressionModule[F] {
  type Result[T]
  type EvoRepr[T] = F[T]

  // TODO it would be nice to make the seed abstract too
  def newSeed: Long

  def interpret[T](expr: Expr[T]): Result[T]

  final def materialize[T](seed: Long, fa: Result[F[T]]): Iterator[T] = materializeWith(seed, fa, emptyCtx)
  def materializeWith[T](seed: Long, fa: Result[F[T]], ctx: Ctx): Iterator[T]
  def materializeConstant[T](t: Result[T]): T
  def materializeConstantWith[T](t: Result[T], ctx: Ctx): T

  final def materializeExpr[T](seed: Long, expr: Expr[F[T]]): Iterator[T] =
    materialize(seed, interpret(expr))

}

private[data] object EvaluationModuleImpl
    extends EvaluationModule[RNGRepr]
    with InterpreterModule
    with ExpressionModule[RNGRepr] {
  override type Result[T] = Out[T]

  override def interpret[T](expr: Expr[T]): Out[T] =
    Interpreter.interpret(expr)
  override def newSeed: Long = Random.nextLong()
  override def materializeWith[T](seed: Long, fa: Result[RNGRepr[T]], ctx: Ctx): Iterator[T] =
    fa(ctx).iterator(RNG(seed))
  override def materializeConstant[T](t: Result[T]): T = materializeConstantWith(t, emptyCtx)
  override def materializeConstantWith[T](t: Result[T], ctx: Ctx): T = t(ctx)
}
