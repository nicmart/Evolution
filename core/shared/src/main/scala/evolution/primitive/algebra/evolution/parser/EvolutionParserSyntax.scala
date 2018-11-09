package evolution.primitive.algebra.evolution.parser
import evolution.primitive.algebra.binding.{Binding, BindingSyntax}
import evolution.primitive.algebra.binding.parser.BindingParserSyntax
import evolution.primitive.algebra.constants.parser.ConstantsParserSyntax
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.{Evolution, EvolutionSyntax}
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.parser.ChainParserSyntax
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi.Parser

class EvolutionParserSyntax[F[_], R[_]](alg: Evolution[F, R, Double, String, String])
    extends EvolutionSyntax[F, ByVarParserK[R, ?], Parser[String]] {

  override val chain: Chain[F, ByVarParserK[R, ?]] =
    new ChainParserSyntax(alg.chain)

  override val constants: Constants[ByVarParserK[R, ?], Unit] =
    new ConstantsParserSyntax[R](alg.constants)

  override val bind: BindingSyntax[ByVarParserK[R, ?], Parser[String], Unit] =
    new BindingParserSyntax(alg.bind)
}
