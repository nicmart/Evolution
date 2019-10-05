package evolution.compiler.expression.typeclass
import evolution.compiler.types.TypeT

sealed abstract class Additive[A, B, C](val t1: TypeT[A], val t2: TypeT[B], val t3: TypeT[C])

object Additive {
  case object DoubleDoubleDouble extends Additive(TypeT.Double, TypeT.Double, TypeT.Double)
  case object IntIntInt extends Additive(TypeT.Integer, TypeT.Integer, TypeT.Integer)
  case object IntDoubleDouble extends Additive(TypeT.Integer, TypeT.Double, TypeT.Double)
  case object DoubleIntDouble extends Additive(TypeT.Double, TypeT.Integer, TypeT.Double)
  case object PointPointPoint extends Additive(TypeT.Point, TypeT.Point, TypeT.Point)
  case class LiftBoth[A, B, C](add: Additive[A, B, C])
      extends Additive(TypeT.Evo(add.t1), TypeT.Evo(add.t2), TypeT.Evo(add.t3))
}
