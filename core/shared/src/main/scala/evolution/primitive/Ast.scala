package evolution.primitive
import evolution.geometry

class Ast[F[_]] {
  sealed trait Expr {
    def tpe: Type
    def withType(tpe: Type): Expr = this match {
      case Expr.Var(name, _)                => Expr.Var(name, tpe)
      case Expr.FuncCall(funcName, args, _) => Expr.FuncCall(funcName, args, tpe)
      case Expr.BinaryOp(op, a, b, _)       => Expr.BinaryOp(op, a, b, tpe)
      case Expr.Lambda(varName, expr, _)    => Expr.Lambda(varName, expr, tpe)
      case Expr.Number(n, _)                => Expr.Number(n, tpe)
    }
  }

  object Expr {
    final case class Var(name: String, tpe: Type = Type.Var("")) extends Expr
    final case class FuncCall(funcName: String, args: List[Expr], tpe: Type = Type.Var("")) extends Expr
    final case class BinaryOp(op: String, a: Expr, b: Expr, tpe: Type = Type.Var("")) extends Expr
    // TODO var should be an expr probably
    final case class Lambda(varName: Expr.Var, expr: Expr, tpe: Type = Type.Var("")) extends Expr
    final case class Number(n: String, tpe: Type = Type.Var("")) extends Expr
  }

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
    def nextTypeVar: Type = Type.Var(nextVar)
  }

  object Context {
    val empty: Context = new Context(Map.empty)
  }
}
