package evolution.compiler.phases.typing.config

import cats.data.{ ReaderT, StateT }
import evolution.compiler.phases.typing.model.TypeInference
import evolution.compiler.types.TypeBindings
import cats.implicits._
import cats.mtl.implicits._

object Instances {

  type TypeInferenceResult[T] = ReaderT[StateT[Either[String, ?], TypeInference.State, ?], TypeBindings, T]
  implicit val typeInference: TypeInference[TypeInferenceResult] = TypeInference.instance[TypeInferenceResult]

  implicit class TypeInferenceOps[T](t: TypeInferenceResult[T]) {
    def evaluateEither(additionalVarTypeBindings: TypeBindings = TypeBindings.empty): Either[String, T] =
      t.run(TypingConfig.constantQualifiedTypes merge additionalVarTypeBindings).runA(TypeInference.empty)

    def unsafeEvaluate: T =
      evaluateEither().fold(
        s => throw new Exception(s),
        identity
      )

    def unsafeEvaluateWith(ctx: TypeBindings): T =
      t.run(TypingConfig.constantQualifiedTypes merge ctx).runA(TypeInference.empty).right.get
  }
}
