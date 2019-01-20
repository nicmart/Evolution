package evolution.primitive
import evolution.geometry

object ast {

  sealed trait Expr
  object Expr {
    final case class Var(name: String) extends Expr
    final case class FuncCall(funcName: String, args: List[Expr]) extends Expr
    final case class BinaryOp(op: String, a: Expr, b: Expr) extends Expr
    final case class Lambda(varName: String, expr: Expr) extends Expr
    final case class Number(n: String) extends Expr
  }

  class Types[F[_]] {
    sealed trait Type { type Out }
    object Type {
      final case class Var(name: String) extends Type { type Out = Nothing }
      final case object Integer extends Type { type Out = Int }
      final case object Dbl extends Type { type Out = Double }
      final case object Point extends Type { type Out = geometry.Point }
      final case object Bool extends Type { type Out = Boolean }
      final case class Evo(inner: Type) extends Type { type Out = F[inner.type] }
      final case class Arrow(from: Type, to: Type) extends Type { type Out = from.type => to.type }
    }

    final class Context(bindings: Map[String, Type]) {
      def has(name: String): Boolean = bindings.isDefinedAt(name)
      def get(name: String): Option[Type] = bindings.get(name)
      def put(name: String, tpe: Type): Context = new Context(bindings.updated(name, tpe))
      def nextVar: String = "X" + bindings.size
    }

    object Context {
      val empty: Context = new Context(Map.empty)
    }
  }
}
