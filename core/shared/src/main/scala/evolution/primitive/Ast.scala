package evolution.primitive
import evolution.geometry

class Ast[F[_]] {
  sealed trait Expr {
    def tpe: TypeAnnotation
    def withType(tpe: Type): Expr = this match {
      case Expr.Var(name, _)                => Expr.Var(name, Typed(tpe))
      case Expr.FuncCall(funcName, args, _) => Expr.FuncCall(funcName, args, Typed(tpe))
      case Expr.BinaryOp(op, a, b, _)       => Expr.BinaryOp(op, a, b, Typed(tpe))
      case Expr.Lambda(varName, expr, _)    => Expr.Lambda(varName, expr, Typed(tpe))
      case Expr.Number(n, _)                => Expr.Number(n, Typed(tpe))
    }
  }

  object Expr {
    final case class Var(name: String, tpe: TypeAnnotation = Unknown) extends Expr
    final case class FuncCall(funcName: String, args: List[Expr], tpe: TypeAnnotation = Unknown) extends Expr
    final case class BinaryOp(op: String, a: Expr, b: Expr, tpe: TypeAnnotation = Unknown) extends Expr
    // TODO var should be an expr probably
    final case class Lambda(varName: Expr.Var, expr: Expr, tpe: TypeAnnotation = Unknown) extends Expr
    final case class Number(n: String, tpe: TypeAnnotation = Unknown) extends Expr
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

  sealed trait TypeAnnotation
  final case object Unknown extends TypeAnnotation
  final case class Typed(tpe: Type) extends TypeAnnotation

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
