package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.binding
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class EvolutionUnshifter[F[_], R[_]](evolution: Evolution[F, R]) extends Evolution.Delegate(evolution) {
  override val bind: Binding[R, String] = new binding.Binding.Delegate(evolution.bind) {
    override def shift[A](expr: R[A]): R[A] = expr
  }
}

object EvolutionUnshifter {
  def unshiftExpr[F[_], T](expr: Expr[F, T]): Expr[F, T] = expr.run(new EvolutionUnshifter(new EvolutionExpr[F]))
}
