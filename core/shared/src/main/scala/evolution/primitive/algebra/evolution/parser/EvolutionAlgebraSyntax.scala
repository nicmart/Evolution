package evolution.primitive.algebra.evolution.parser
import evolution.geometry.Point
import evolution.primitive.algebra.{ByVarParser, Composed}
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.binding.parser.BindingAlgebraSyntax
import evolution.primitive.algebra.constants.parser.ConstantsAlgebraSyntax
import evolution.primitive.algebra.constants.{ConstantsAlgebra, ContextualConstantsAlgebra}
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.{ContextualListAlgebra, ListAlgebra}
import evolution.primitive.algebra.list.parser.ListAlgebraSyntax
import fastparse.noApi.Parser

class EvolutionAlgebraSyntax[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String])
    extends EvolutionAlgebra[S, F, ByVarParser[R, ?], Parser[String]] {

  private val constantsSyntax = new ConstantsAlgebraSyntax[Composed[R, S, ?]](alg.constants)
  private val bindingSyntax = new BindingAlgebraSyntax(alg.bind)

  override val list: ListAlgebra[S, F, ByVarParser[R, ?]] =
    new ContextualListAlgebra[S, F, λ[α => Parser[R[α]]], List[String]](new ListAlgebraSyntax(alg.list))

  override val constants: ConstantsAlgebra[λ[α => ByVarParser[R, S[α]]]] =
    new ContextualConstantsAlgebra[λ[α => Parser[R[S[α]]]], List[String]](constantsSyntax)

  override val bind: BindingAlgebra[ByVarParser[R, ?], Parser[String]] =
    bindingSyntax

  val doubleConstant: ByVarParser[R, S[Double]] = _ => constantsSyntax.anyDouble
  val variable: Parser[String] = bindingSyntax.variableIdentifier
}
