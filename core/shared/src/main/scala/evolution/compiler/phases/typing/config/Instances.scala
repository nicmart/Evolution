package evolution.compiler.phases.typing.config

import evolution.compiler.phases.typing.model.TypeInference
import cats.implicits._
import cats.mtl.implicits._

object Instances {

  type TypeInferenceResult[T] = Either[String, T]
  implicit val typeInference: TypeInference[TypeInferenceResult] = TypeInference.instance[TypeInferenceResult]

  implicit class TypeInferenceOps[T](t: TypeInferenceResult[T]) {

    def unsafeEvaluate: T =
      t.fold(
        s => throw new Exception(s),
        identity
      )
  }
}
