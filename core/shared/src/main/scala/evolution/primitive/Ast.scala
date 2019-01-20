package evolution.primitive
import evolution.geometry

class Ast[F[_]] {
  sealed trait Expr {
    def tpe: TypeAnnotation
  }

  object Expr {
    final case class Var(name: String, tpe: TypeAnnotation = Unknown) extends Expr
    final case class FuncCall(funcName: String, args: List[Expr], tpe: TypeAnnotation = Unknown) extends Expr
    final case class BinaryOp(op: String, a: Expr, b: Expr, tpe: TypeAnnotation = Unknown) extends Expr
    final case class Lambda(varName: String, expr: Expr, tpe: TypeAnnotation = Unknown) extends Expr
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
  }

  object Context {
    val empty: Context = new Context(Map.empty)
  }
}
