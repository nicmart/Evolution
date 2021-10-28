package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type

enum Comparable[T](val t: Type):
  case Int extends Comparable[Int](Type.Integer)
  case Double extends Comparable[Double](Type.Double)
