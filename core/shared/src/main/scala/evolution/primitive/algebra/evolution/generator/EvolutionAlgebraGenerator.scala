package evolution.primitive.algebra.evolution.generator
import evolution.generator.Generator
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.generator.BindingGenerator
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.constants.generator.ConstantsAlgebraGenerator
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.list.generator.ListAlgebraGenerator
import org.scalacheck.Gen

class EvolutionAlgebraGenerator[S[_], F[_], R[_], VarName](alg: EvolutionAlgebra[S, F, R, VarName])
    extends EvolutionAlgebra[S, F, GenRepr[R, ?], Generator[VarName]] {

  override val list: ListAlgebra[S, F, GenRepr[R, ?]] =
    new ListAlgebraGenerator(alg.list)

  override val constants: ConstantsAlgebra[GenRepr[Composed[R, S, ?], ?]] =
    new ConstantsAlgebraGenerator[Composed[R, S, ?]](alg.constants)

  override val bind: Binding[GenRepr[R, ?], Generator[VarName]] =
    new BindingGenerator(alg.bind)
}
