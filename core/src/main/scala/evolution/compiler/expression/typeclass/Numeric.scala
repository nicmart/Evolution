package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type

enum Numeric[T](val t: Type):
  case Int extends Numeric[Int](Type.Integer)
  case Double extends Numeric[Double](Type.Double)
