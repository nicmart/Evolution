package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

sealed abstract class Additive[A, B, C](val t1: Type, val t2: Type, val t3: Type)

object Additive {
  case object DoubleDoubleDouble extends Additive[Double, Double, Double](Type.Double, Type.Double, Type.Double)
  case object IntIntInt extends Additive[Int, Int, Int](Type.Integer, Type.Integer, Type.Integer)
  case object IntDoubleDouble extends Additive[Int, Double, Double](Type.Integer, Type.Double, Type.Double)
  case object DoubleIntDouble extends Additive[Double, Int, Double](Type.Double, Type.Integer, Type.Double)
  case object PointPointPoint extends Additive[Point, Point, Point](Type.Point, Type.Point, Type.Point)
  case class LiftLeft[A, B, C](add: Additive[A, B, C])
      extends Additive[Evolution[A], B, Evolution[C]](Type.Evo(add.t1), add.t2, Type.Evo(add.t3))
  case class LiftRight[A, B, C](add: Additive[A, B, C])
      extends Additive[A, Evolution[B], Evolution[C]](add.t1, Type.Evo(add.t2), Type.Evo(add.t3))
  case class LiftBoth[A, B, C](add: Additive[A, B, C])
      extends Additive[Evolution[A], Evolution[B], Evolution[C]](Type.Evo(add.t1), Type.Evo(add.t2), Type.Evo(add.t3))
}
