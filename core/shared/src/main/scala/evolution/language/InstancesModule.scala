package evolution.language
import cats.data.{ ReaderT, StateT }
import cats.implicits._
import cats.mtl.implicits._

trait InstancesModule[F[_]] { self: TyperModule[F] =>
  import Typer._
  type TypeInferenceResult[T] = ReaderT[StateT[Either[String, ?], TypeInference.State, ?], TypeContext, T]
  implicit val typeInference: TypeInference[TypeInferenceResult] = instance[TypeInferenceResult]

  implicit class TypeInferenceOps[T](t: TypeInferenceResult[T]) {
    def evaluateEither: Either[String, T] =
      t.run(constantQualifiedTypes).runA(TypeInference.empty).value

    def unsafeEvaluate: T =
      evaluateEither.fold(
        s => throw new Exception(s),
        identity
      )

    def unsafeEvaluateWith(ctx: TypeContext): T =
      t.run(constantQualifiedTypes ++ ctx).runA(TypeInference.empty).value.right.get
  }
}
