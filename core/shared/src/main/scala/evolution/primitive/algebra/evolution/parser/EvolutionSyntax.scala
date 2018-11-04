package evolution.primitive.algebra.evolution.parser
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.parser.BindingSyntax
import evolution.primitive.algebra.constants.parser.ConstantsSyntax
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.parser.ChainSyntax
import evolution.primitive.algebra.parser.ByVarParser.ByVarParserK
import fastparse.noApi.Parser

class EvolutionSyntax[F[_], R[_]](alg: Evolution[F, R, Double, String, String])
    extends Evolution[F, ByVarParserK[R, ?], Unit, Parser[String], Unit] {

  override val list: Chain[F, ByVarParserK[R, ?]] =
    new ChainSyntax(alg.list)

  override val constants: Constants[ByVarParserK[R, ?], Unit] =
    new ConstantsSyntax[R](alg.constants)

  override val bind: Binding[ByVarParserK[R, ?], Parser[String], Unit] =
    new BindingSyntax(alg.bind)
}
