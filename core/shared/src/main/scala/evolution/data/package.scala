package evolution
import evolution.materialization.Iterable

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextModuleImpl
  type Ctx = EvaluationContext.Ctx
  final val EvaluationModule: EvaluationModule[Iterable] = EvaluationModuleImpl
}
