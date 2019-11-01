package evolution.compiler.types
import evolution.compiler.expression.Expr

case class Typed[T](tpe: Type, value: Expr[T])
