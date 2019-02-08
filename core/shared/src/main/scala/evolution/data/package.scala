package evolution
import evolution.algebra.representation.RNGRepr

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextDebugModuleImpl
  type Ctx = EvaluationContext.Ctx
  final val EvaluationModule: EvaluationModule[RNGRepr] = EvaluationModuleImpl
}
