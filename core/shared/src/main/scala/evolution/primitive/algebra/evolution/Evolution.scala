package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.{ Binding, BindingSyntax }
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.distribution.Distribution

trait Evolution[F[_], R[_], D, Var, VarName] {
  val chain: Chain[F, R]
  val constants: Constants[R, D]
  val bind: Binding[R, Var, VarName]
  val distribution: Distribution[F, R]
  val derived: Derived[F, R]
}

trait EvolutionSyntax[F[_], R[_], Var] {
  val chain: Chain[F, R]
  val constants: Constants[R, Unit]
  val bind: BindingSyntax[R, Var, Unit]
  val distribution: Distribution[F, R]
  val derived: Derived[F, R]
}

object Evolution {
  trait Expr[F[_], T] {
    def run[R[_]](alg: Evolution[F, R, Double, String, String]): R[T]
  }
}
