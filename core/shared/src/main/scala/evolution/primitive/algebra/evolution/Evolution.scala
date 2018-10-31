package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.chain.Chain

trait Evolution[S[_], F[_], R[_], D, Var, VarName] {
  val list: Chain[S, F, R]
  val constants: Constants[Composed[R, S, ?], D]
  val bind: Binding[R, Var, VarName]
}

object Evolution {
  trait Expr[S[_], F[_], T] {
    def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[T]
  }
}
