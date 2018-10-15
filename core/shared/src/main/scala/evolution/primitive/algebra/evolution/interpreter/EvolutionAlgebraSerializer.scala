package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.{ConstString, CtxString}
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingSerializer
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.constants.interpreter.ConstantsAlgebraSerializer
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.list.interpreter.ListAlgebraSerializer

// TODO missing tests
object EvolutionAlgebraSerializer extends EvolutionAlgebra[ConstString, ConstString, CtxString, String] {
  override val list: ListAlgebra[ConstString, ConstString, CtxString] = ListAlgebraSerializer
  override val constants: ConstantsAlgebra[CtxString] = ConstantsAlgebraSerializer
  override val bind: Binding[CtxString, String] = BindingSerializer
}
