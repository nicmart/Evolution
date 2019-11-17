package evolution.compiler.impl.evaluation

private[evolution] final case class EvalCtx(items: Map[String, () => Any]) extends AnyVal {
  def get[T](name: String): T = items(name)().asInstanceOf[T]
  def addLazy[T](name: String, value: () => T): EvalCtx = EvalCtx(items.updated(name, value))
  def addStrict[T](name: String, value: T): EvalCtx = addLazy(name, () => value)
}

private[evolution] object EvalCtx {
  val emptyCtx: EvalCtx = EvalCtx(Map.empty)
  def ctxOf(bindings: (String, Any)*): EvalCtx = bindings.foldRight(emptyCtx) {
    case ((name, value), ctx) => ctx.addStrict(name, value)
  }
}
