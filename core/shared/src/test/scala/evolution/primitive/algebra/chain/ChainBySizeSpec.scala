package evolution.primitive.algebra.chain

import evolution.generator.instances.GeneratorInstances
import evolution.primitive.algebra.{Const, ConstString, CtxString, GenRepr, Sized, TestInterpreters}
import evolution.primitive.algebra.chain.generator.ChainGenerator
import _root_.evolution.primitive.algebra.chain.interpreter.{ChainBySize, ChainSerializer}
import org.scalatest.{FreeSpec, Matchers}
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class ChainBySizeSpec
    extends FreeSpec
    with Matchers
    with TestInterpreters
    with GeneratorInstances
    with GeneratorDrivenPropertyChecks {
  //val sizeEvaluator = ChainSizeEvaluator

  type S[T] = ConstString[T]
  type F[T] = ConstString[T]
  type R[T] = CtxString[T]

  type GenOfSize[T] = GenRepr[Const[?, Int], T]
  type GenOfSizeBySize[T] = Sized[GenOfSize, T]

  val serializer: Chain[ConstString, ConstString, CtxString] = ChainSerializer
  val generator: Chain[S, F, GenRepr[R, ?]] = new ChainGenerator(serializer)

  val sizedGenerator: Chain[S, F, Sized[GenRepr[R, ?], ?]] =
    new ChainBySize[S, F, GenRepr[R, ?]](generator, genOrMonoidK[R])
}
