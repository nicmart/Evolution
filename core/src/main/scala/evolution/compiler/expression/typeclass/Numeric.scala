package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClassInstance

enum Numeric[T](val t: Type, val materialized: Int => T):
  def instance: TypeClassInstance = TypeClassInstance("Num", List(t), materialized)
  case Int extends Numeric[Int](Type.Integer, identity)
  case Double extends Numeric[Double](Type.Double, _.toDouble)
