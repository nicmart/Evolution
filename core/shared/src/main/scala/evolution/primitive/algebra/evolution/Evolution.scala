package evolution.primitive.algebra.evolution
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.binding.{ Binding, BindingSyntax }
import evolution.primitive.algebra.constants.{ Constants, ConstantsSyntax }
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.derived.Derived
import evolution.primitive.algebra.distribution.Distribution
import fastparse.noApi._

trait Evolution[F[_], R[_]] {
  val chain: Chain[F, R]
  val constants: Constants[R]
  val bind: Binding[R, String]
  val distribution: Distribution[F, R]
  val derived: Derived[F, R]
}

trait EvolutionSyntax[F[_], R[_]] {
  val chain: Chain[F, R]
  val constants: ConstantsSyntax[R]
  val bind: BindingSyntax[R, Parser[String]]
  val distribution: Distribution[F, R]
  val derived: Derived[F, R]
}

object Evolution {
  trait Expr[F[_], T] {
    def run[R[_]](alg: Evolution[F, R]): R[T]
  }

  class Delegate[F[_], R[_]](evolution: Evolution[F, R]) extends Evolution[F, R] {
    override val chain: Chain[F, R] = evolution.chain
    override val constants: Constants[R] = evolution.constants
    override val bind: Binding[R, String] = evolution.bind
    override val distribution: Distribution[F, R] = evolution.distribution
    override val derived: Derived[F, R] = evolution.derived
  }
}
