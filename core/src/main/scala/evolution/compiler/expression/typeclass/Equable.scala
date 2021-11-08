package evolution.compiler.expression.typeclass
import evolution.compiler.types.Type
import evolution.geometry.Point
import cats.kernel.Eq
import evolution.compiler.types.TypeClassInstance

enum Equable[T](val t: Type, val materialized: Eq[T]):
  def instance: TypeClassInstance = TypeClassInstance("Eq", List(t), materialized)
  case Double extends Equable[Double](Type.Double, Eq[Double])
  case Int extends Equable[Int](Type.Integer, Eq[Int])
  case Point extends Equable[Point](Type.Point, Eq[Point])
  case Boolean extends Equable[Boolean](Type.Bool, Eq[Boolean])
