package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingExpr
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainExpr
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsExpr
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.derived.interpreter.DerivedExpr
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.distribution.interpreter.DistributionExpr
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class EvolutionExpr[F[_]] extends Evolution[F, Expr[F, ?], String, String] {
  override val chain: Chain[F, Expr[F, ?]] = new ChainExpr[F]
  override val constants: Constants[Expr[F, ?]] = new ConstantsExpr[F]
  override val bind: Binding[Expr[F, ?], String, String] = new BindingExpr[F]
  override val distribution: Distribution[F, Expr[F, ?]] = new DistributionExpr[F]
  override val derived: Derived[F, Expr[F, ?]] = new DerivedExpr[F]
}
