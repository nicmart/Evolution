package evolution.primitive.algebra.evolution.generator
import evolution.primitive.algebra.{Composed, Generator}
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.binding.generator.BindingAlgebraGenerator
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.constants.generator.ConstantsAlgebraGenerator
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.list.generator.ListAlgebraGenerator
import org.scalacheck.Gen

class EvolutionAlgebraGenerator[S[_], F[_], R[_], VarName](alg: EvolutionAlgebra[S, F, R, VarName])
    extends EvolutionAlgebra[S, F, Generator[R, ?], Gen[VarName]] {

  override val list: ListAlgebra[S, F, Generator[R, ?]] =
    new ListAlgebraGenerator(alg.list)

  override val constants: ConstantsAlgebra[Generator[Composed[R, S, ?], ?]] =
    new ConstantsAlgebraGenerator[Composed[R, S, ?]](alg.constants)

  override val bind: BindingAlgebra[Generator[R, ?], Gen[VarName]] =
    new BindingAlgebraGenerator(alg.bind)
}
