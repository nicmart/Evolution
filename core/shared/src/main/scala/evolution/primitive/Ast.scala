package evolution.primitive
import enumeratum.{ Enum, EnumEntry }
import evolution.geometry

import scala.collection.immutable

class Ast[F[_]] {
  sealed trait Expr {

    def tpe: Type

    def withType(tpe: Type): Expr = this match {
      case Expr.Var(name, _)                => Expr.Var(name, tpe)
      case Expr.FuncCall(funcName, args, _) => Expr.FuncCall(funcName, args, tpe)
      case Expr.Lambda(varName, expr, _)    => Expr.Lambda(varName, expr, tpe)
      case Expr.Number(n, _)                => Expr.Number(n, tpe)
    }

    def children: List[Expr] = this match {
      case Expr.Var(name, tpe)                => Nil
      case Expr.FuncCall(funcName, args, tpe) => args
      case Expr.Lambda(varName, expr, tpe)    => List(varName, expr)
      case Expr.Number(n, tpe)                => Nil
    }
  }

  object Expr {
    final case class Var(name: String, tpe: Type = Type.Var("")) extends Expr
    final case class FuncCall(funcId: PredefinedFunction, args: List[Expr], tpe: Type = Type.Var("")) extends Expr
    // TODO should this be just a func call?
    final case class Lambda(varName: Expr.Var, expr: Expr, tpe: Type = Type.Var("")) extends Expr
    final case class Number(n: String, tpe: Type = Type.Var("")) extends Expr
  }

  abstract sealed class PredefinedFunction(val arity: Int) extends EnumEntry

  object PredefinedFunction extends Enum[PredefinedFunction] {
    val values: immutable.IndexedSeq[PredefinedFunction] = findValues

    // Constants
    case object Point extends PredefinedFunction(2)
    case object Add extends PredefinedFunction(2)
    case object Inverse extends PredefinedFunction(1)
    case object Multiply extends PredefinedFunction(2)
    case object Sin extends PredefinedFunction(1)
    case object Cos extends PredefinedFunction(1)
    case object Eq extends PredefinedFunction(2)
    case object If extends PredefinedFunction(3)

    // Bindings
    case object Fix extends PredefinedFunction(1)
    case object App extends PredefinedFunction(2)
    //case object Let extends PredefinedFunction(1)

    // Chain
    case object Empty extends PredefinedFunction(0)
    case object Cons extends PredefinedFunction(2)
    case object MapEmpty extends PredefinedFunction(2)
    case object MapCons extends PredefinedFunction(2)

    // Derived
    case object Cartesian extends PredefinedFunction(2)
    case object Polar extends PredefinedFunction(2)
    case object Constant extends PredefinedFunction(1)
    case object Integrate extends PredefinedFunction(2)
    case object Solve1 extends PredefinedFunction(2)
    case object Solve2 extends PredefinedFunction(2)
    case object Concat extends PredefinedFunction(2)
    case object Map extends PredefinedFunction(2)
    case object FlatMap extends PredefinedFunction(2)
    case object Take extends PredefinedFunction(2)

    // Distribution
    case object Uniform extends PredefinedFunction(2)
  }

  sealed trait Type {
    type Out
    def children: List[Type] = this match {
      case Type.Evo(inner)      => List(inner)
      case Type.Arrow(from, to) => List(from, to)
      case _                    => Nil
    }
  }
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
