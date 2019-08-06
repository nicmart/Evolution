package evolution

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextModuleImpl
  type Ctx = EvaluationContext.Ctx
  final val EvaluationModule: EvaluationModule = EvaluationModuleImpl
  type EvaluationResult[T] = EvaluationModuleImpl.Result[T]
}
