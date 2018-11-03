package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.chain.Chain

trait Evolution[F[_], R[_], D, Var, VarName] {
  val list: Chain[F, R]
  val constants: Constants[R, D]
  val bind: Binding[R, Var, VarName]
}

object Evolution {
  trait Expr[F[_], T] {
    def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[T]
  }
}
