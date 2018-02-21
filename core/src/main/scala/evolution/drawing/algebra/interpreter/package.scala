package evolution.drawing.algebra

import evolution.algebra
import evolution.algebra.Evolution

package object interpreter {
  type ConstString[-E, +A] = String
  type CtxString[-E, +A] = List[String] => String
  type CtxEvolution[-E, +A] = E => StaticOrDynamicEvolution[A]

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
