package evolution
import evolution.data.EvaluationContextModuleImpl

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextDebugModuleImpl
  type Ctx = EvaluationContext.Ctx
}
