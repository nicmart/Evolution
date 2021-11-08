package evolution.compiler.expression.typeclass
import cats.kernel.Order
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClassInstance

enum Comparable[T](val t: Type, val materialized: Order[T]):
  def instance: TypeClassInstance = TypeClassInstance("Comp", List(t), materialized)
  case Int extends Comparable[Int](Type.Integer, Order[Int])
  case Double extends Comparable[Double](Type.Double, Order[Double])
