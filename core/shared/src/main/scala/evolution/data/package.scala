package evolution
import evolution.materialization.Iterable

package object data {
  import evolution.materialization.RNGRepr
  final val EvaluationContext: EvaluationContextModule = EvaluationContextModuleImpl
  type Ctx = EvaluationContext.Ctx
  //final val EvaluationModule: EvaluationModule[RNGRepr] = RNGReprEvaluationModuleImpl
  final val EvaluationModule: EvaluationModule[Iterable] = IterableEvaluationModuleImpl
}
