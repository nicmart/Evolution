package evolution.compiler.types
import evolution.compiler.expression.Expr

case class Typed[T](tpe: TypeT[T], value: Expr[T])
