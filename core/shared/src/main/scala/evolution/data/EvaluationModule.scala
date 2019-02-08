package evolution.data
import cats.{ Applicative, Id }
import evolution.algebra.representation.RNGRepr
import evolution.data.EvaluationContext._
import evolution.data.EvaluationContextModule._
import evolution.random.RNG
import evolution.primitive.InterpreterModule

import scala.util.Random

trait EvaluationModule[F[_]] extends WithExpression[F] {
  type Result[T]

  import expressionModule._

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
    with WithExpression[RNGRepr] {
  import expressionModule._
  override type Result[T] = Out[T]

  override def interpret[T](expr: Expr[T]): Out[T] =
    Interpreter.interpret(expr)
  override def newSeed: Long = Random.nextLong()
  override def materializeWith[T](seed: Long, fa: Result[RNGRepr[T]], ctx: Ctx): Iterator[T] =
    fa(ctx).iterator(RNG(seed))
  override def materializeConstant[T](t: Result[T]): T = materializeConstantWith(t, emptyCtx)
  override def materializeConstantWith[T](t: Result[T], ctx: Ctx): T = t(ctx)
}
