package evolution.primitive.algebra.evolution.generator
import evolution.generator.Generator
import evolution.primitive.algebra.{ Composed, GenRepr }
import evolution.primitive.algebra.binding.{ Binding, BindingSyntax }
import evolution.primitive.algebra.binding.generator.BindingGenerator
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.generator.ConstantsGenerator
import evolution.primitive.algebra.evolution.{ Evolution, EvolutionSyntax }
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.generator.ChainGenerator
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.distribution.Distribution

class EvolutionGenerator[F[_], R[_], Var](alg: Evolution[F, R, Double, Var, String])
    extends EvolutionSyntax[F, GenRepr[R, ?], Generator[Var]] {

  override val chain: Chain[F, GenRepr[R, ?]] =
    new ChainGenerator(alg.chain)

  override val constants: Constants[GenRepr[R, ?], Unit] =
    new ConstantsGenerator[R](alg.constants)

  override val bind: BindingSyntax[GenRepr[R, ?], Generator[Var], Unit] =
    new BindingGenerator(alg.bind)

  override val distribution: Distribution[F, GenRepr[R, ?]] = ???

  override val derived: Derived[F, GenRepr[R, ?]] = ???
}
