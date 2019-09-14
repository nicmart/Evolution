package evolution.compiler.expression.typeclass
import evolution.compiler.types.TypeT

sealed abstract class Multiplicative[A, B, C](val t1: TypeT[A], val t2: TypeT[B], val t3: TypeT[C])

object Multiplicative {
  case object DoubleDoubleDouble extends Multiplicative(TypeT.Double, TypeT.Double, TypeT.Double)
  case object DoublePointPoint extends Multiplicative(TypeT.Double, TypeT.Point, TypeT.Point)
  case object PointDoublePoint extends Multiplicative(TypeT.Point, TypeT.Double, TypeT.Point)
  case object IntIntInt extends Multiplicative(TypeT.Integer, TypeT.Integer, TypeT.Integer)
  case object IntDoubleDouble extends Multiplicative(TypeT.Integer, TypeT.Double, TypeT.Double)
  case object DoubleIntDouble extends Multiplicative(TypeT.Double, TypeT.Integer, TypeT.Double)
  case object IntPointPoint extends Multiplicative(TypeT.Integer, TypeT.Point, TypeT.Point)
  case object PointIntPoint extends Multiplicative(TypeT.Point, TypeT.Integer, TypeT.Point)

  case class LiftLeft[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative(TypeT.Evo(m.t1), m.t2, TypeT.Evo(m.t3))
  case class LiftRight[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative(m.t1, TypeT.Evo(m.t2), TypeT.Evo(m.t3))
  case class LiftBoth[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative(TypeT.Evo(m.t1), TypeT.Evo(m.t2), TypeT.Evo(m.t3))
}
