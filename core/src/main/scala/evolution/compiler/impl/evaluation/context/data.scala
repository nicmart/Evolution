package evolution.compiler.impl.evaluation

package object context {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextModuleImpl
  type Ctx = EvaluationContext.Ctx
  final val emptyCtx: Ctx = EvaluationContext.emptyCtx
}
