package evolution
import evolution.data.EvaluationResultModuleImpl

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationResultModuleImpl
  type Ctx = EvaluationContext.Ctx
}
