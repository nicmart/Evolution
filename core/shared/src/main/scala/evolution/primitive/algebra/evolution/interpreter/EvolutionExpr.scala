package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingExpr
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainExpr
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsExpr
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class EvolutionExpr[S[_], F[_]] extends Evolution[S, F, Expr[S, F, ?], Double, String, String] {
  override val list: Chain[S, F, Expr[S, F, ?]] = new ChainExpr[S, F]
  override val constants: Constants[Composed[Expr[S, F, ?], S, ?], Double] = new ConstantsExpr[S, F]
  override val bind: Binding[Expr[S, F, ?], String, String] = new BindingExpr[S, F]
}
