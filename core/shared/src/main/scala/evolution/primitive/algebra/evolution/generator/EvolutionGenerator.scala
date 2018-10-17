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

class EvolutionGenerator[S[_], F[_], R[_], Var](alg: Evolution[S, F, R, Double, Var])
    extends Evolution[S, F, GenRepr[R, ?], Unit, Generator[Var]] {

  override val list: Chain[S, F, GenRepr[R, ?]] =
    new ChainGenerator(alg.list)

  override val constants: Constants[GenRepr[Composed[R, S, ?], ?], Unit] =
    new ConstantsGenerator[Composed[R, S, ?]](alg.constants)

  override val bind: Binding[GenRepr[R, ?], Generator[Var]] =
    new BindingGenerator(alg.bind)
}
