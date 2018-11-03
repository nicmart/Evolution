package evolution.primitive.algebra.evolution.parser
import evolution.primitive.algebra.{ByVarParser, Composed}
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.parser.BindingSyntax
import evolution.primitive.algebra.constants.parser.ConstantsSyntax
import evolution.primitive.algebra.constants.{Constants, ContextualConstants}
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.{ContextualChain, Chain}
import evolution.primitive.algebra.chain.parser.ChainSyntax
import fastparse.noApi.Parser

class EvolutionSyntax[F[_], R[_]](alg: Evolution[F, R, Double, String, String])
    extends Evolution[F, ByVarParser[R, ?], Unit, Parser[String], Unit] {

  private val constantsSyntax = new ConstantsSyntax[R](alg.constants)
  private val bindingSyntax = new BindingSyntax(alg.bind)

  override val list: Chain[F, ByVarParser[R, ?]] =
    new ContextualChain[F, λ[α => Parser[R[α]]], List[String]](new ChainSyntax(alg.list))

  override val constants: Constants[ByVarParser[R, ?], Unit] =
    new ContextualConstants[λ[α => Parser[R[α]]], Unit, List[String]](constantsSyntax)

  override val bind: Binding[ByVarParser[R, ?], Parser[String], Unit] =
    bindingSyntax
}
