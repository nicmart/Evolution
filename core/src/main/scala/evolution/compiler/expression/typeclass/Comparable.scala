package evolution.compiler.expression.typeclass
import cats.kernel.Order
import evolution.compiler.types.Type

enum Comparable[T](val t: Type, val materialized: Order[T]):
  case Int extends Comparable[Int](Type.Integer, Order[Int])
  case Double extends Comparable[Double](Type.Double, Order[Double])
