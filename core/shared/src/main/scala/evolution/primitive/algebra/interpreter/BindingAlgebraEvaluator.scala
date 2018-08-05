package evolution.primitive.algebra.interpreter
import evolution.primitive.algebra.BindingAlgebra

object BindingAlgebraEvaluator extends BindingAlgebra[Ctx] {
  override def var0[A]: Ctx[A] = {
    case h :: tail => h().asInstanceOf[A]
  }
  override def shift[A](expr: Ctx[A]): Ctx[A] = {
    case h :: tail => expr(tail)
  }
  override def let[A, B](name: String, value: Ctx[A])(expr: Ctx[B]): Ctx[B] =
    ctx => expr((() => value(ctx)) :: ctx)

  override def fix[A](expr: Ctx[A]): Ctx[A] =
    ctx => expr((() => fix(expr)(ctx)) :: ctx)

  override def lambda[A, B](name: String, expr: Ctx[B]): Ctx[B] = expr

  override def app[A, B](f: Ctx[A], b: Ctx[B]): Ctx[A] = ctx => {
    f((() => b(ctx)) :: ctx)
  }
}

object BindingAlgebraDebugEvaluator extends BindingAlgebra[Ctx] {
  private val b = BindingAlgebraEvaluator
  override def var0[A]: Ctx[A] = debug("var0", b.var0[A])
  override def shift[A](expr: Ctx[A]): Ctx[A] = debug("shift", b.shift[A](expr))
  override def let[A, B](name: String, value: Ctx[A])(expr: Ctx[B]): Ctx[B] =
    debug(s"let $name", b.let[A, B](name, value)(expr))

  override def fix[A](expr: Ctx[A]): Ctx[A] = debug[A]("fix", b.fix(expr))

  override def lambda[A, B](name: String, expr: Ctx[B]): Ctx[B] = debug(s"lambda $name", b.lambda[A, B](name, expr))

  override def app[A, B](f: Ctx[A], bb: Ctx[B]): Ctx[A] = debug("app", b.app(f, bb))

  def debug[A](name: String, expr: Ctx[A]): Ctx[A] = ctx => {
    println(s"BEGIN $name evaluation with Ctx size ${ctx.size}")
    val result = expr(ctx)
    println(s"END $name evaluation end: ${result}")
    result
  }
}
