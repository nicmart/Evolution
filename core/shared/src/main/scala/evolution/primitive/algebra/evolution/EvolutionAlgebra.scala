package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.{BindingAlgebra, CoreDrawingAlgebra, ScalarAlgebra}

trait EvolutionAlgebra[S[_], F[_], R[_], VarName] {
  final type RS[T] = R[S[T]]
  final type RF[T] = R[F[T]]
  val drawing: CoreDrawingAlgebra[S, F, R]
  val scalar: ScalarAlgebra[RS]
  val bind: BindingAlgebra[R, VarName]
}
