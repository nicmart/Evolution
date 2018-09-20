package evolution.primitive.algebra.interpreter
import evolution.primitive.algebra.BindingAlgebra

object BindingAlgebraEvaluator extends BindingAlgebra[EvaluationResult, String] {
  override def varName(name: String): String = name

  override def var0[A]: EvaluationResult[A] = Value {
    case h :: tail => h().asInstanceOf[A]
  }

  override def shift[A](expr: EvaluationResult[A]): EvaluationResult[A] = Value {
    case h :: tail => expr.get(tail)
  }
  override def let[A, B](name: String, value: EvaluationResult[A])(expr: EvaluationResult[B]): EvaluationResult[B] = {
    val ctxValue = value.get
    val ctxExpr = expr.get
    Value[B](ctx => ctxExpr((() => ctxValue(ctx)) :: ctx))
  }

  override def fix[A](expr: EvaluationResult[A => A]): EvaluationResult[A] =
    expr match {
      case Lambda(term, _) => Value(fixTerm(term))
      case _ => app(expr, fix(expr))
    }

  override def lambda[A, B](name: String, expr: EvaluationResult[B]): EvaluationResult[A => B] = {
    val ctxExpr = expr.get
    Lambda(ctxExpr, ctx => a => ctxExpr((() => a) :: ctx))
  }

  override def app[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]): EvaluationResult[B] =
    Value(ctx => f.get(ctx)(a.get(ctx)))

  private def fixTerm[A](expr: Ctx[A]): Ctx[A] =
    ctx => expr((() => fixTerm(expr)(ctx)) :: ctx)
}

object BindingAlgebraDebugEvaluator extends BindingAlgebra[EvaluationResult, String] {
  private val b = BindingAlgebraEvaluator
  override def varName(name: String): String = name
  override def var0[A]: EvaluationResult[A] = debug("var0", b.var0[A])
  override def shift[A](expr: EvaluationResult[A]): EvaluationResult[A] = debug("shift", b.shift[A](expr))
  override def let[A, B](name: String, value: EvaluationResult[A])(expr: EvaluationResult[B]): EvaluationResult[B] =
    debug(s"let $name", b.let[A, B](name, value)(expr))

  override def fix[A](expr: EvaluationResult[A => A]): EvaluationResult[A] = debug[A]("fix", b.fix(expr))

  override def lambda[A, B](name: String, expr: EvaluationResult[B]): EvaluationResult[A => B] =
    debug(s"lambda $name", b.lambda[A, B](name, expr))

  override def app[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]): EvaluationResult[B] =
    debug("app", b.app(f, a))

  // TODO
  def debug[A](name: String, expr: EvaluationResult[A]): EvaluationResult[A] = expr
//    ctx => {
//      println(s"BEGIN $name evaluation with EvaluationResult size ${ctx.size}")
//      val result = expr(ctx)
//      println(s"END $name evaluation end: ${result}")
//      result
//    }
}

sealed trait EvaluationResult[T] {
  def get: Ctx[T]
}
case class Lambda[A, B](term: Ctx[B], get: Ctx[A => B]) extends EvaluationResult[A => B]
case class Value[A](get: Ctx[A]) extends EvaluationResult[A]
