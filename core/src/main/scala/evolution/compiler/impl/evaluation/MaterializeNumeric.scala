package evolution.compiler.impl.evaluation

import evolution.compiler.expression.typeclass.Numeric

object MaterializeNumeric:
  def apply[A](numeric: Numeric[A]): Int => A =
    numeric match
      case Numeric.Int    => n => n
      case Numeric.Double => n => n.toDouble
