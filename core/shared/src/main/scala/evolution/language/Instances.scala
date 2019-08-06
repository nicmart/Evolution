package evolution.language
import cats.data.{ ReaderT, StateT }
import cats.implicits._
import cats.mtl.implicits._
import evolution.language.Typer.TypeInference
import evolution.language.Typer.TypeContext

object Instances {

  type TypeInferenceResult[T] = ReaderT[StateT[Either[String, ?], TypeInference.State, ?], TypeContext, T]
  implicit val typeInference: TypeInference[TypeInferenceResult] = Typer.instance[TypeInferenceResult]

  implicit class TypeInferenceOps[T](t: TypeInferenceResult[T]) {
    def evaluateEither(additionalVarTypeBindings: TypeContext = Map.empty): Either[String, T] =
      t.run(Typer.constantQualifiedTypes ++ additionalVarTypeBindings).runA(TypeInference.empty)

    def unsafeEvaluate: T =
      evaluateEither().fold(
        s => throw new Exception(s),
        identity
      )

    def unsafeEvaluateWith(ctx: TypeContext): T =
      t.run(Typer.constantQualifiedTypes ++ ctx).runA(TypeInference.empty).right.get
  }
}
