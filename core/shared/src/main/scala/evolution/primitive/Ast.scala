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

  abstract sealed class PredefinedFunction(val arity: Int) extends EnumEntry

  object PredefinedFunction extends Enum[PredefinedFunction] {
    val values: immutable.IndexedSeq[PredefinedFunction] = findValues

    // Constants
    case object Point extends PredefinedFunction(2)
    case object X extends PredefinedFunction(1)
    case object Y extends PredefinedFunction(1)
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
