package evolution.primitive.algebra.evolution.parser
import evolution.primitive.algebra.constants.parser.{ConstantsAlgebraSyntax}
import evolution.primitive.algebra.constants.{ConstantsAlgebra, ContextualConstantsAlgebra}
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.parser.BindingAlgebra.ByVarParser
import evolution.primitive.algebra.parser.{BindingAlgebra, CoreDrawingAlgebraSyntax}
import evolution.primitive.algebra.{BindingAlgebra, ContextualCoreDrawingAlgebra, CoreDrawingAlgebra}
import fastparse.noApi.Parser

class EvolutionAlgebraSyntax[S[_], F[_], R[_]](alg: EvolutionAlgebra[S, F, R, String])
    extends EvolutionAlgebra[S, F, ByVarParser[R, ?], Parser[String]] {

  override val drawing: CoreDrawingAlgebra[S, F, ByVarParser[R, ?]] =
    new ContextualCoreDrawingAlgebra[S, F, λ[α => Parser[R[α]]], List[String]](
      new CoreDrawingAlgebraSyntax(alg.drawing)
    )

  override val scalar: ConstantsAlgebra[λ[α => ByVarParser[R, S[α]]]] =
    new ContextualConstantsAlgebra[λ[α => Parser[R[S[α]]]], List[String]](new ConstantsAlgebraSyntax(alg.scalar))

  override val bind: BindingAlgebra[ByVarParser[R, ?], Parser[String]] =
    new BindingAlgebra.Syntax(alg.bind)
}
