package evolution.compiler.impl.evaluation.context

case class Context(items: Map[String, () => Any]) {
  def get[T](name: String): T = items.get(name).asInstanceOf[T]
  def addLazy[T](name: String, value: () => T): Context = Context(items.updated(name, value))
  def addStrict[T](name: String, value: T): Context = addLazy(name, () => value)
}

sealed trait EvaluationContextModule {
  type Ctx
  def emptyCtx: Ctx
  def addLazy[T](name: String, value: () => T, ctx: Ctx): Ctx
  def addStrict[T](name: String, value: T, ctx: Ctx): Ctx = addLazy(name, () => value, ctx)
  def get[T](ctx: Ctx, name: String): T
  def ctxOf(bindings: (String, Any)*): Ctx = bindings.foldRight(emptyCtx) {
    case ((name, value), ctx) =>
      addStrict(name, value, ctx)
  }
}

object EvaluationContextModule {
  implicit class CtxOps(val ctx: Ctx) extends AnyVal {
    def addLazy[T](name: String, value: () => T): Ctx = EvaluationContext.addLazy(name, value, ctx)
    def addStrict[T](name: String, elem: T): Ctx = addLazy(name, () => elem)
  }
}

private[context] object EvaluationContextModuleImpl extends EvaluationContextModule {
  override type Ctx = Map[String, () => Any]
  @inline override def emptyCtx: Ctx = Map.empty
  @inline override def addLazy[T](name: String, value: () => T, ctx: Ctx): Ctx = ctx.updated(name, value)
  @inline override def get[T](ctx: Ctx, name: String): T = ctx.apply(name)().asInstanceOf[T]
}
