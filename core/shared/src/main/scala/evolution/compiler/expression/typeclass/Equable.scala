package evolution.compiler.expression.typeclass
import evolution.geometry.Point

sealed trait Equable[T]

object Equable {
  case object DblEquable extends Equable[Double]
  case object IntEquable extends Equable[Int]
  case object PointEquable extends Equable[Point]
}
