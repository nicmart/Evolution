package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.list.ListAlgebra

trait EvolutionAlgebra[S[_], F[_], R[_], VarName] {
  final type RS[T] = R[S[T]]
  val list: ListAlgebra[S, F, R]
  val constants: ConstantsAlgebra[Composed[R, S, ?]]
  val bind: Binding[R, VarName]
}
