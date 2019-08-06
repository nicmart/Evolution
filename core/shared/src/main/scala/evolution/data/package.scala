package evolution
import evolution.materialization.Evolution

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextModuleImpl
  type Ctx = EvaluationContext.Ctx
  final val EvaluationModule: EvaluationModule[Evolution] = EvaluationModuleImpl
}
