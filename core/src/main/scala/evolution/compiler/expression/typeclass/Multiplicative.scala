package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution
import evolution.compiler.types.TypeClassInstance

enum Multiplicative[A, B, C](val t1: Type, val t2: Type, val t3: Type, val materialized: (A, B) => C):
  def instance: TypeClassInstance = TypeClassInstance("Mult", List(t1, t2, t3), materialized)
  case DoubleDoubleDouble extends Multiplicative[Double, Double, Double](Type.Double, Type.Double, Type.Double, _ * _)
  case DoublePointPoint
      extends Multiplicative[Double, Point, Point](Type.Double, Type.Point, Type.Point, (a, b) => b.mult(a))
  case PointDoublePoint
      extends Multiplicative[Point, Double, Point](Type.Point, Type.Double, Type.Point, (a, b) => a.mult(b))
  case IntIntInt extends Multiplicative[Int, Int, Int](Type.Integer, Type.Integer, Type.Integer, _ * _)
  case IntDoubleDouble extends Multiplicative[Int, Double, Double](Type.Integer, Type.Double, Type.Double, _ * _)
  case DoubleIntDouble extends Multiplicative[Double, Int, Double](Type.Double, Type.Integer, Type.Double, _ * _)
  case IntPointPoint
      extends Multiplicative[Int, Point, Point](Type.Integer, Type.Point, Type.Point, (a, b) => b.mult(a))
  case PointIntPoint
      extends Multiplicative[Point, Int, Point](Type.Point, Type.Integer, Type.Point, (a, b) => a.mult(b))
  case LiftLeft[A, B, C](inner: Multiplicative[A, B, C])
      extends Multiplicative[Evolution[A], B, Evolution[C]](
        Type.Evo(inner.t1),
        inner.t2,
        Type.Evo(inner.t3),
        (a, b) => Evolution.map(a, aa => inner.materialized(aa, b))
      )
  case LiftRight[A, B, C](inner: Multiplicative[A, B, C])
      extends Multiplicative[A, Evolution[B], Evolution[C]](
        inner.t1,
        Type.Evo(inner.t2),
        Type.Evo(inner.t3),
        (a, b) => Evolution.map(b, bb => inner.materialized(a, bb))
      )
  case LiftBoth[A, B, C](inner: Multiplicative[A, B, C])
      extends Multiplicative[Evolution[A], Evolution[B], Evolution[C]](
        Type.Evo(inner.t1),
        Type.Evo(inner.t2),
        Type.Evo(inner.t3),
        (a, b) => Evolution.zipWithUncurried(inner.materialized)(a, b)
      )
