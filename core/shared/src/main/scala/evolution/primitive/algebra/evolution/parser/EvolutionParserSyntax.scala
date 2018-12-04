package evolution.primitive.algebra.evolution.parser
import evolution.primitive.algebra.binding.{ Binding, BindingSyntax }
import evolution.primitive.algebra.binding.parser.BindingParserSyntax
import evolution.primitive.algebra.constants.parser.ConstantsParserSyntax
import evolution.primitive.algebra.constants.{ Constants, ConstantsSyntax }
import evolution.primitive.algebra.evolution.{ Evolution, EvolutionSyntax }
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.parser.ChainParserSyntax
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.derived.parser.DerivedParserSyntax
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.distribution.parser.DistributionParserSyntax
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi.Parser

class EvolutionParserSyntax[F[_], R[_]](alg: Evolution[F, R]) extends EvolutionSyntax[F, ByVarParserK[R, ?]] {

  override val chain: Chain[F, ByVarParserK[R, ?]] =
    new ChainParserSyntax(alg.chain)

  override val constants: ConstantsSyntax[ByVarParserK[R, ?]] =
    new ConstantsParserSyntax[R](alg.constants)

  override val bind: BindingSyntax[ByVarParserK[R, ?], Parser[String]] =
    new BindingParserSyntax(alg.bind)

  override val distribution: Distribution[F, ByVarParserK[R, ?]] =
    new DistributionParserSyntax(alg.distribution)

  override val derived: Derived[F, ByVarParserK[R, ?]] =
    new DerivedParserSyntax(alg.derived)
}