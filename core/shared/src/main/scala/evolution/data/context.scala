package evolution.data

sealed trait EvaluationContextModule {
  type Ctx
  def emptyCtx: Ctx
  def pushLazy[T](elem: () => T, ctx: Ctx, debugMessage: String): Ctx
  def pushStrict[T](elem: T, ctx: Ctx, debugMessage: String): Ctx = pushLazy(() => elem, ctx, debugMessage)
  def pop(ctx: Ctx): Ctx
  def get[T](ctx: Ctx, n: Int): T
  def ctxOf(elems: Any*): Ctx = elems.foldRight(emptyCtx) { (elem, ctx) =>
    pushStrict(elem, ctx, "")
  }
}

object EvaluationContextModule {
  implicit class CtxOps(val ctx: Ctx) extends AnyVal {
    def pushLazy[T](elem: () => T, debugMessage: String): Ctx = EvaluationContext.pushLazy(elem, ctx, debugMessage)
    def pushStrict[T](elem: T, debugMessage: String): Ctx = pushLazy(() => elem, debugMessage)
    def pop: Ctx = EvaluationContext.pop(ctx)
    def apply[T](n: Int): T = EvaluationContext.get(ctx, n)
  }
}

private[data] object EvaluationContextModuleImpl extends EvaluationContextModule {
  override type Ctx = List[() => Any]
  @inline override def emptyCtx: Ctx = Nil
  @inline override def pushLazy[T](elem: () => T, ctx: Ctx, debugMessage: String): Ctx = elem :: ctx
  @inline override def pop(ctx: Ctx): List[() => Any] = ctx.tail
  @inline override def get[T](ctx: List[() => Any], n: Int): T = ctx.apply(n)().asInstanceOf[T]
}

private[data] object EvaluationContextDebugModuleImpl extends EvaluationContextModule {
  override type Ctx = List[Result[_]]
  @inline override def emptyCtx: Ctx = Nil
  @inline override def pushLazy[T](elem: () => T, ctx: Ctx, debugMessage: String): Ctx =
    Result(elem, debugMessage) :: ctx
  @inline override def pop(ctx: Ctx): Ctx = ctx.tail
  @inline override def get[T](ctx: Ctx, n: Int): T = {
    val result = ctx.apply(n)
    result.get().asInstanceOf[T]
  }
  case class Result[T](get: () => T, label: String)
}
