package evolution.primitive.algebra.binding.interpreter
import cats.Applicative
import evolution.primitive.algebra.Ctx
import evolution.primitive.algebra.binding.Binding

object BindingEvaluator extends Binding[EvaluationResult, String, String] {
  override def v(name: String): String = name

  override def var0[A]: EvaluationResult[A] = Value {
    case h :: tail => h().asInstanceOf[A]
  }

  override def shift[A](expr: EvaluationResult[A]): EvaluationResult[A] = {
    val exprCtx = expr.get
    Value(ctx => exprCtx(ctx.tail))
  }
  override def let[A, B](name: String, value: EvaluationResult[A], expr: EvaluationResult[B]): EvaluationResult[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: EvaluationResult[A => A]): EvaluationResult[A] =
    expr match {
      case Lambda(term, _) => Value(fixTerm(term.get))
      case _ => app(expr, fix(expr))
    }

  override def lambda[A, B](name: String, expr: EvaluationResult[B]): EvaluationResult[A => B] = {
    val ctxExpr = expr.get
    Lambda(expr, ctx => a => ctxExpr((() => a) :: ctx))
  }

  override def app[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]): EvaluationResult[B] = {
    val (fCtx, aCtx) = (f.get, a.get)
    Value(ctx => fCtx(ctx)(aCtx(ctx)))
  }

  private def fixTerm[A](expr: Ctx[A]): Ctx[A] =
    ctx => expr((() => fixTerm(expr)(ctx)) :: ctx)
}

sealed trait EvaluationResult[T] {
  def get: Ctx[T]
}

object EvaluationResult {
  implicit val applicative: Applicative[EvaluationResult] = new Applicative[EvaluationResult] {
    override def pure[A](x: A): EvaluationResult[A] = Value(_ => x)
    override def ap[A, B](ff: EvaluationResult[A => B])(fa: EvaluationResult[A]): EvaluationResult[B] =
      BindingEvaluator.app(ff, fa)
  }
}

case class Lambda[A, B](term: EvaluationResult[B], get: Ctx[A => B]) extends EvaluationResult[A => B]
case class Value[A](get: Ctx[A]) extends EvaluationResult[A]
