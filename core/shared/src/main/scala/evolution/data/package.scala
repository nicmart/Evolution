package evolution

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextModuleImpl
  type Ctx = EvaluationContext.Ctx
  final val emptyCtx: Ctx = EvaluationContext.emptyCtx
  final val EvaluationModule: EvaluationModule = EvaluationModuleImpl
}
