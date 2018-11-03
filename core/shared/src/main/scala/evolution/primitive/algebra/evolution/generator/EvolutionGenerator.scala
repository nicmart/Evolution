package evolution.primitive.algebra.evolution.generator
import evolution.generator.Generator
import evolution.primitive.algebra.{Composed, GenRepr}
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.generator.BindingGenerator
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.generator.ConstantsGenerator
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.generator.ChainGenerator

class EvolutionGenerator[F[_], R[_], Var](alg: Evolution[F, R, Double, Var, String])
    extends Evolution[F, GenRepr[R, ?], Unit, Generator[Var], Unit] {

  override val list: Chain[F, GenRepr[R, ?]] =
    new ChainGenerator(alg.list)

  override val constants: Constants[GenRepr[R, ?], Unit] =
    new ConstantsGenerator[R](alg.constants)

  override val bind: Binding[GenRepr[R, ?], Generator[Var], Unit] =
    new BindingGenerator(alg.bind)
}
