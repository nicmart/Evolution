package evolution
import evolution.data.EvaluationContextModuleImpl

package object data {
  final val EvaluationContext: EvaluationContextModule = EvaluationContextDebugModuleImpl
  type Ctx = EvaluationContext.Ctx

  final val EvaluationModule: EvaluationModule = EvaluationModuleImpl
  final val AnnotationModule: AnnotationModule = AnnotationModuleImpl
  type Annotation[T] = AnnotationModule.Annotation[T]
}
