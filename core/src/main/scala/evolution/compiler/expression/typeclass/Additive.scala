package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

enum Additive[A, B, C](val t1: Type, val t2: Type, val t3: Type):
  case DoubleDoubleDouble extends Additive[Double, Double, Double](Type.Double, Type.Double, Type.Double)
  case IntIntInt extends Additive[Int, Int, Int](Type.Integer, Type.Integer, Type.Integer)
  case IntDoubleDouble extends Additive[Int, Double, Double](Type.Integer, Type.Double, Type.Double)
  case DoubleIntDouble extends Additive[Double, Int, Double](Type.Double, Type.Integer, Type.Double)
  case PointPointPoint extends Additive[Point, Point, Point](Type.Point, Type.Point, Type.Point)
  case LiftLeft[A, B, C](add: Additive[A, B, C])
      extends Additive[Evolution[A], B, Evolution[C]](Type.Evo(add.t1), add.t2, Type.Evo(add.t3))
  case LiftRight[A, B, C](add: Additive[A, B, C])
      extends Additive[A, Evolution[B], Evolution[C]](add.t1, Type.Evo(add.t2), Type.Evo(add.t3))
  case LiftBoth[A, B, C](add: Additive[A, B, C])
      extends Additive[Evolution[A], Evolution[B], Evolution[C]](Type.Evo(add.t1), Type.Evo(add.t2), Type.Evo(add.t3))
