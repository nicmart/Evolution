package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.{ConstString, CtxString}
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingSerializer
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.constants.interpreter.ConstantsSerializer
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.chain.interpreter.ChainSerializer

// TODO missing tests
class EvolutionSerializer[S[_], F[_]] extends Evolution[S, F, CtxString, Double, String, String] {
  override val list: Chain[S, F, CtxString] = new ChainSerializer
  override val constants: Constants[CtxString, Double] = ConstantsSerializer
  override val bind: Binding[CtxString, String, String] = BindingSerializer
}
