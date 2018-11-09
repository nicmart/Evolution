package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.{Binding, BindingSyntax}
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.chain.Chain

trait Evolution[F[_], R[_], D, Var, VarName] {
  val chain: Chain[F, R]
  val constants: Constants[R, D]
  val bind: Binding[R, Var, VarName]
}

trait EvolutionSyntax[F[_], R[_], Var] {
  val chain: Chain[F, R]
  val constants: Constants[R, Unit]
  val bind: BindingSyntax[R, Var, Unit]
}

object Evolution {
  trait Expr[F[_], T] {
    def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[T]
  }
}
