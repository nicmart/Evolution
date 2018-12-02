package evolution.data

sealed trait EvaluationContextModule {
  type Ctx
  def emptyCtx: Ctx
  def pushLazy[T](elem: () => T, ctx: Ctx): Ctx
  def pushStrict[T](elem: T, ctx: Ctx): Ctx = pushLazy(() => elem, ctx)
  def pop(ctx: Ctx): Ctx
  def get[T](ctx: Ctx, n: Int): T
  def ctxOf(elems: Any*): Ctx = elems.foldRight(emptyCtx) { (elem, ctx) =>
    pushStrict(elem, ctx)
  }
}

object EvaluationContextModule {
  implicit class CtxOps(val ctx: Ctx) extends AnyVal {
    def pushLazy[T](elem: () => T): Ctx = EvaluationContext.pushLazy(elem, ctx)
    def pushStrict[T](elem: T): Ctx = EvaluationContext.pushStrict(elem, ctx)
    def pop: Ctx = EvaluationContext.pop(ctx)
    def apply[T](n: Int): T = EvaluationContext.get(ctx, n)
  }
}

private[data] object EvaluationResultModuleImpl extends EvaluationContextModule {
  override type Ctx = List[() => Any]
  @inline override def emptyCtx: Ctx = Nil
  @inline override def pushLazy[T](elem: () => T, ctx: Ctx): Ctx = elem :: ctx
  @inline override def pop(ctx: Ctx): List[() => Any] = ctx.tail
  @inline override def get[T](ctx: List[() => Any], n: Int): T = ctx.apply(n)().asInstanceOf[T]
}
