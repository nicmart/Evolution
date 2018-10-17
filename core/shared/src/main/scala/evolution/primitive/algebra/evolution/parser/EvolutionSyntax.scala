package evolution.primitive.algebra.evolution.parser
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, Composed}
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.parser.BindingSyntax
import evolution.primitive.algebra.constants.parser.ConstantsSyntax
import evolution.primitive.algebra.constants.{Constants, ContextualConstants}
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.{ContextualChain, Chain}
import evolution.primitive.algebra.chain.parser.ChainSyntax
import fastparse.noApi.Parser

class EvolutionSyntax[S[_], F[_], R[_]](alg: Evolution[S, F, R, Double, String, String])
    extends Evolution[S, F, ByVarParser[R, ?], Unit, Parser[String], Unit] {

  private val constantsSyntax = new ConstantsSyntax[Composed[R, S, ?]](alg.constants)
  private val bindingSyntax = new BindingSyntax(alg.bind)

  override val list: Chain[S, F, ByVarParser[R, ?]] =
    new ContextualChain[S, F, λ[α => Parser[R[α]]], List[String]](new ChainSyntax(alg.list))

  override val constants: Constants[λ[α => ByVarParser[R, S[α]]], Unit] =
    new ContextualConstants[λ[α => Parser[R[S[α]]]], Unit, List[String]](constantsSyntax)

  override val bind: Binding[ByVarParser[R, ?], Parser[String], Unit] =
    bindingSyntax
}
