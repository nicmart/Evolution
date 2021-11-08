package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution
import evolution.compiler.types.TypeClassInstance

enum Additive[A, B, C](val t1: Type, val t2: Type, val t3: Type, val materialized: (A, B) => C):
  def instance: TypeClassInstance = TypeClassInstance("Add", List(t1, t2, t3), materialized)
  
  case DoubleDoubleDouble extends Additive[Double, Double, Double](Type.Double, Type.Double, Type.Double, _ + _)
  case IntIntInt extends Additive[Int, Int, Int](Type.Integer, Type.Integer, Type.Integer, _ + _)
  case IntDoubleDouble extends Additive[Int, Double, Double](Type.Integer, Type.Double, Type.Double, _ + _)
  case DoubleIntDouble extends Additive[Double, Int, Double](Type.Double, Type.Integer, Type.Double, _ + _)
  case PointPointPoint extends Additive[Point, Point, Point](Type.Point, Type.Point, Type.Point, _ + _)
  case LiftLeft[A, B, C](inner: Additive[A, B, C])
      extends Additive[Evolution[A], B, Evolution[C]](
        Type.Evo(inner.t1),
        inner.t2,
        Type.Evo(inner.t3),
        (a, b) => Evolution.map(a, aa => inner.materialized(aa, b))
      )
  case LiftRight[A, B, C](inner: Additive[A, B, C])
      extends Additive[A, Evolution[B], Evolution[C]](
        inner.t1,
        Type.Evo(inner.t2),
        Type.Evo(inner.t3),
        (a, b) => Evolution.map(b, bb => inner.materialized(a, bb))
      )
  case LiftBoth[A, B, C](inner: Additive[A, B, C])
      extends Additive[Evolution[A], Evolution[B], Evolution[C]](
        Type.Evo(inner.t1),
        Type.Evo(inner.t2),
        Type.Evo(inner.t3),
        (a, b) => Evolution.zipWithUncurried(inner.materialized)(a, b)
      )


