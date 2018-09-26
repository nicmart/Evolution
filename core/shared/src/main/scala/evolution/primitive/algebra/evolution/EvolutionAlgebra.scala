package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.BindingAlgebra
import evolution.primitive.algebra.list.ListAlgebra

trait EvolutionAlgebra[S[_], F[_], R[_], VarName] {
  final type RS[T] = R[S[T]]
  final type RF[T] = R[F[T]]
  val drawing: ListAlgebra[S, F, R]
  val scalar: ConstantsAlgebra[RS]
  val bind: BindingAlgebra[R, VarName]
}
