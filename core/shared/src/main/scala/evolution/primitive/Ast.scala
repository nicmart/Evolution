package evolution.primitive
import cats.{ Eq, Group }
import cats.kernel.Semigroup
import enumeratum.{ Enum, EnumEntry }
import evolution.geometry
import evolution.primitive.algebra.evolution.Evolution
import cats.implicits._
import evolution.geometry.Point
import evolution.typeclass.VectorSpace

import scala.collection.immutable

class Ast[F[_]] {

  sealed trait Expr {
    val tpe: Type
    final type Out = tpe.Out

    def withType(tpe: Type): Expr = this match {
      case Expr.Var(name, _)                => Expr.Var(name, tpe)
      case Expr.FuncCall(funcName, args, _) => Expr.FuncCall(funcName, args, tpe)
      case Expr.Lambda(varName, expr, _)    => Expr.Lambda(varName, expr, tpe)
      case Expr.Let(varName, expr, in, _)   => Expr.Let(varName, expr, in, tpe)
      case Expr.Number(n, _)                => Expr.Number(n, tpe)
    }

    def children: List[Expr] = this match {
      case Expr.Var(name, _)                => Nil
      case Expr.FuncCall(funcName, args, _) => args
      case Expr.Lambda(varName, expr, _)    => List(varName, expr)
      case Expr.Let(varName, expr, in, _)   => List(varName, expr, in)
      case Expr.Number(n, _)                => Nil
    }
  }

  object Expr {
    final case class Var(name: String, tpe: Type = Type.Var("")) extends Expr
    final case class FuncCall(funcId: PredefinedFunction, args: List[Expr], tpe: Type = Type.Var("")) extends Expr
    final case class Lambda(varName: Expr.Var, expr: Expr, tpe: Type = Type.Var("")) extends Expr
    final case class Let(varName: Expr.Var, expr: Expr, in: Expr, tpe: Type = Type.Var("")) extends Expr
    final case class Number(n: String, tpe: Type = Type.Var("")) extends Expr
  }

  abstract sealed class PredefinedFunction extends EnumEntry

  object PredefinedFunction extends Enum[PredefinedFunction] {
    val values: immutable.IndexedSeq[PredefinedFunction] = findValues

    // Constants
    case object Point extends PredefinedFunction
    case object X extends PredefinedFunction
    case object Y extends PredefinedFunction
    case object Add extends PredefinedFunction
    case object Div extends PredefinedFunction
    case object Exp extends PredefinedFunction
    case object Inverse extends PredefinedFunction
    case object Multiply extends PredefinedFunction
    case object Sin extends PredefinedFunction
    case object Cos extends PredefinedFunction
    case object Eq extends PredefinedFunction
    case object If extends PredefinedFunction

    // Bindings
    case object Fix extends PredefinedFunction
    case object App extends PredefinedFunction
    //case object Let extends PredefinedFunction

    // Chain
    case object Empty extends PredefinedFunction
    case object Cons extends PredefinedFunction
    case object MapEmpty extends PredefinedFunction
    case object MapCons extends PredefinedFunction

    // Derived
    case object Cartesian extends PredefinedFunction
    case object Polar extends PredefinedFunction
    case object Constant extends PredefinedFunction
    case object Integrate extends PredefinedFunction
    case object Solve1 extends PredefinedFunction
    case object Solve2 extends PredefinedFunction
    case object Concat extends PredefinedFunction
    case object Map extends PredefinedFunction
    case object FlatMap extends PredefinedFunction
    case object Take extends PredefinedFunction

    // Distribution
    case object Uniform extends PredefinedFunction
    case object UniformDiscrete extends PredefinedFunction
    case object UniformChoice extends PredefinedFunction
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
    final case class Var(name: String) extends Type {
      type Out = Nothing
      override def toString: String = name
    }
    final case object Integer extends Type { type Out = Int }
    final case object Dbl extends Type { type Out = Double }
    final case object Point extends Type { type Out = geometry.Point }
    final case object Bool extends Type { type Out = Boolean }
    final case class Evo(inner: Type) extends Type { type Out = F[inner.type] }
    final case class Arrow(from: Type, to: Type) extends Type { type Out = from.type => to.type }

    def group(t: Type): Either[String, Group[t.Out]] = {
      t match {
        case Type.Integer => Right(Group[Int])
        case Type.Dbl     => Right(Group[Double])
        case Type.Point   => Right(Group[Point])
        case _            => Left(s"Unable to find a group for type $t")
      }
    }.asInstanceOf[Either[String, Group[t.Out]]]

    def vectorSpace(t: Type): Either[String, VectorSpace[t.Out]] = {
      t match {
        case Type.Integer => Right(VectorSpace[Int])
        case Type.Dbl     => Right(VectorSpace[Double])
        case Type.Point   => Right(VectorSpace[Point])
        case _            => Left(s"Unable to find a vector space for type $t")
      }
    }.asInstanceOf[Either[String, VectorSpace[t.Out]]]

    def eqTypeClass(t: Type): Either[String, Eq[t.Out]] = {
      t match {
        case Type.Integer => Right(Eq[Int])
        case Type.Dbl     => Right(Eq[Double])
        case Type.Point   => Right(Eq[Point])
        case _            => Left(s"Unable to find a vector space for type $t")
      }
    }.asInstanceOf[Either[String, Eq[t.Out]]]

    def unwrapF(t: Type): Either[String, Type] = t match {
      case Type.Evo(inner) => Right(inner)
      case _               => Left(s"Type $t is not an Evolution type")
    }
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

trait WithAst[F[_]] {
  val ast: Ast[F] = new Ast
}
