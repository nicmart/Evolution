package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import evolution.materialization.Evolution

enum Invertible[T](val t: Type):
  case Int extends Invertible[Int](Type.Integer)
  case Double extends Invertible[Double](Type.Double)
  case Point extends Invertible[Point](Type.Point)
  case Lift[T](inv: Invertible[T]) extends Invertible[Evolution[T]](Type.Evo(inv.t))
