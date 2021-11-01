package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

enum Invertible[T](val t: Type, val materialized: T => T):
  case Int extends Invertible[Int](Type.Integer, -_)
  case Double extends Invertible[Double](Type.Double, -_)
  case Point extends Invertible[Point](Type.Point, -_)
  case Lift[T](inner: Invertible[T])
      extends Invertible[Evolution[T]](Type.Evo(inner.t), t => Evolution.map(t, tt => inner.materialized(tt)))
