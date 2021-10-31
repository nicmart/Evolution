package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type

enum Numeric[T](val t: Type, val liftInt: Int => T):
  case Int extends Numeric[Int](Type.Integer, identity)
  case Double extends Numeric[Double](Type.Double, _.toDouble)
