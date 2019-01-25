package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.{ ConstString, CtxString }
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingSerializer
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsSerializer
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainSerializer
import evolution.primitive.algebra.derived.{ DefaultDerived, Derived }
import evolution.primitive.algebra.derived.interpreter.DerivedSerializer
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.distribution.interpreter.DistributionSerializer

// TODO missing tests
class EvolutionSerializer[F[_]] extends Evolution[F, CtxString] {
  override val chain: Chain[F, CtxString] = new ChainSerializer
  override val constants: Constants[CtxString] = ConstantsSerializer
  override val bind: Binding[CtxString, String] = BindingSerializer
  override val distribution: Distribution[F, CtxString] = new DistributionSerializer
  override val derived: Derived[F, CtxString] = new DerivedSerializer[F]
}

class DesugarEvolutionSerializer[F[_]] extends EvolutionSerializer[F] {
  override val derived: Derived[F, CtxString] = new DefaultDerived[F, CtxString](new EvolutionSerializer[F])
}

class DeBrujinEvolutionSerializer[F[_]] extends EvolutionSerializer[F] {
  override val bind: Binding[CtxString, String] = new BindingSerializer {
    override def var0[A](name: String): CtxString[A] =
      ctx => "var(0)"
    override def shift[A](expr: CtxString[A]): CtxString[A] =
      ctx => s"shift(${expr(ctx)})"
  }
}
