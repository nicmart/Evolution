package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

enum Multiplicative[A, B, C](val t1: Type, val t2: Type, val t3: Type):
  case DoubleDoubleDouble extends Multiplicative[Double, Double, Double](Type.Double, Type.Double, Type.Double)
  case DoublePointPoint extends Multiplicative[Double, Point, Point](Type.Double, Type.Point, Type.Point)
  case PointDoublePoint extends Multiplicative[Point, Double, Point](Type.Point, Type.Double, Type.Point)
  case IntIntInt extends Multiplicative[Int, Int, Int](Type.Integer, Type.Integer, Type.Integer)
  case IntDoubleDouble extends Multiplicative[Int, Double, Double](Type.Integer, Type.Double, Type.Double)
  case DoubleIntDouble extends Multiplicative[Double, Int, Double](Type.Double, Type.Integer, Type.Double)
  case IntPointPoint extends Multiplicative[Int, Point, Point](Type.Integer, Type.Point, Type.Point)
  case PointIntPoint extends Multiplicative[Point, Int, Point](Type.Point, Type.Integer, Type.Point)
  case LiftLeft[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative[Evolution[A], B, Evolution[C]](Type.Evo(m.t1), m.t2, Type.Evo(m.t3))
  case LiftRight[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative[A, Evolution[B], Evolution[C]](m.t1, Type.Evo(m.t2), Type.Evo(m.t3))
  case LiftBoth[A, B, C](m: Multiplicative[A, B, C])
      extends Multiplicative[Evolution[A], Evolution[B], Evolution[C]](Type.Evo(m.t1), Type.Evo(m.t2), Type.Evo(m.t3))
