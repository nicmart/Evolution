package evolution.data
import evolution.compiler.phases.materializing.Materialize
import evolution.compiler.phases.materializing.model.Contextual
import evolution.materialization.Evolution

import scala.util.Random

trait EvaluationModule {
  type Result[T]

  // TODO it would be nice to make the seed abstract too
  def newSeed: Long

  def interpret[T](expr: Expr[T]): Result[T]

  final def materialize[T](seed: Long, fa: Result[Evolution[T]]): Iterator[T] = materializeWith(seed, fa, emptyCtx)
  def materializeWith[T](seed: Long, fa: Result[Evolution[T]], ctx: Ctx): Iterator[T]
  def materializeConstant[T](t: Result[T]): T
  def materializeConstantWith[T](t: Result[T], ctx: Ctx): T

  final def materializeExpr[T](seed: Long, expr: Expr[Evolution[T]]): Iterator[T] =
    materialize(seed, interpret(expr))

}

private[data] object EvaluationModuleImpl extends EvaluationModule {
  override type Result[T] = Contextual[T]

  override def interpret[T](expr: Expr[T]): Result[T] =
    Materialize.materialize(expr)
  override def newSeed: Long = Random.nextLong()
  override def materializeWith[T](seed: Long, fa: Result[Evolution[T]], ctx: Ctx): Iterator[T] = {
    Random.setSeed(seed)
    fa(ctx).run
  }
  override def materializeConstant[T](t: Result[T]): T = materializeConstantWith(t, emptyCtx)
  override def materializeConstantWith[T](t: Result[T], ctx: Ctx): T = t(ctx)
}
