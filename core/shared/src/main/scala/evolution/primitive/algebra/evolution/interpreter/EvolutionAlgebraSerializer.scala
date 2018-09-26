package evolution.primitive.algebra.evolution.interpreter
import evolution.primitive.algebra.{ConstString, CtxString}
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.binding.interpreter.BindingAlgebraSerializer
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.constants.interpreter.ConstantsAlgebraSerializer
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.ListAlgebra
import evolution.primitive.algebra.list.interpreter.ListAlgebraSerializer

object EvolutionAlgebraSerializer extends EvolutionAlgebra[ConstString, ConstString, CtxString, String] {
  override val drawing: ListAlgebra[ConstString, ConstString, CtxString] = ListAlgebraSerializer
  override val constants: ConstantsAlgebra[RS] = ConstantsAlgebraSerializer
  override val bind: BindingAlgebra[CtxString, String] = BindingAlgebraSerializer
}
