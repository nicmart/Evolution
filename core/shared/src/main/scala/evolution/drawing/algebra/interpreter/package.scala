package evolution.drawing.algebra

import _root_.evolution.algebra
import _root_.evolution.algebra.Evolution

package object interpreter {
  type CtxString[A] = List[String] => String
  type CtxEvolution[+A] = List[Any] => StaticOrDynamicEvolution[A]

  sealed trait StaticOrDynamicEvolution[+A] {
    def evolution: Evolution[A]
  }
  final case class Static[+A](value: A) extends StaticOrDynamicEvolution[A] {
    override def evolution: Evolution[A] = new Evolution[A] {
      override def run[Evo[+ _]](implicit alg: algebra.FullAlgebra[Evo]): Evo[A] = alg.constant(value)
    }
  }
  final case class Dynamic[+A](evolution: Evolution[A]) extends StaticOrDynamicEvolution[A]
}
