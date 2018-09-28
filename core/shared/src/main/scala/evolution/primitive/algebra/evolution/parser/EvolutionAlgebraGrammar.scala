package evolution.primitive.algebra.evolution.parser
import evolution.primitive.algebra.evolution.EvolutionAlgebra

trait EvolutionAlgebraGrammar[S[_], F[_], R[_], VarName] extends EvolutionAlgebra[S, F, R, VarName] {
  def evolutionOfDoubles: R[F[Double]]
  def evolutionPoint: R[F[Double]]
}
