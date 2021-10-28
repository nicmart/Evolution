package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point

enum Equable[T](val t: Type):
  case Double extends Equable[Double](Type.Double)
  case Int extends Equable[Int](Type.Integer)
  case Point extends Equable[Point](Type.Point)
  case Boolean extends Equable[Boolean](Type.Bool)
