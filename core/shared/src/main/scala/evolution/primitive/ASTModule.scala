package evolution.primitive
import cats.{ Eq, Group }
import cats.kernel.Semigroup
import enumeratum.{ Enum, EnumEntry }
import evolution.geometry
import cats.implicits._
import evolution.geometry.Point
import evolution.typeclass.VectorSpace

import scala.collection.immutable

class ASTModule[F[_]] {
  import TypeClasses._
  sealed trait AST {
    val tpe: Type
    final type Out = tpe.Out

    def withType(tpe: Type): AST = this match {
      case AST.Var(name, _)              => AST.Var(name, tpe)
      case AST.Const(id, _, ps)          => AST.Const(id, tpe, ps)
      case AST.App(f, x, _)              => AST.App(f, x, tpe)
      case AST.Lambda(varName, expr, _)  => AST.Lambda(varName, expr, tpe)
      case AST.Let(varName, expr, in, _) => AST.Let(varName, expr, in, tpe)
      case AST.Number(n, _)              => AST.Number(n, tpe)
    }

    def children: List[AST] = this match {
      case AST.Lambda(varName, expr, _)  => List(varName, expr)
      case AST.Let(varName, expr, in, _) => List(varName, expr, in)
      case AST.App(f, x, _)              => List(f, x)
      case _                             => Nil
    }
  }

  object AST {
    final case class Var(name: String, tpe: Type = Type.Var("")) extends AST
    final case class Lambda(varName: AST.Var, expr: AST, tpe: Type = Type.Var("")) extends AST
    final case class App(f: AST, x: AST, tpe: Type = Type.Var("")) extends AST
    // TODO predicates should not be here
    final case class Const(id: PredefinedConstant, tpe: Type = Type.Var(""), predicates: List[Predicate] = Nil)
        extends AST
    final case class Let(varName: AST.Var, expr: AST, in: AST, tpe: Type = Type.Var("")) extends AST
    final case class Number(n: String, tpe: Type = Type.Var("")) extends AST

    def Lift(ast: AST): AST = App(Const(PredefinedConstant.Lift), ast)
    def App2(f: AST, x: AST, y: AST): AST = App(App(f, x), y)
    def App3(f: AST, x: AST, y: AST, z: AST): AST = App(App(App(f, x), y), z)

    // Note: f is not (and can't) be applied to variables
    def transformChildren(tree: AST, f: AST => AST): AST = tree match {
      case Lambda(varName, expr, tpe)  => Lambda(varName, f(expr), tpe)
      case Let(varName, expr, in, tpe) => Let(varName, f(expr), f(in), tpe)
      case App(g, x, tpe)              => App(f(g), f(x), tpe)
      case _                           => tree
    }

    def transformRecursively(tree: AST, f: AST => AST): AST =
      f(transformChildren(tree, transformRecursively(_, f)))
  }

  // TODO this name will evolve to "Constants"
  abstract sealed class PredefinedConstant(val scheme: Type, val predicates: List[Predicate]) extends EnumEntry
  abstract sealed class PredefinedFunction(scheme: Type) extends PredefinedConstant(scheme, Nil)

  object PredefinedConstant extends Enum[PredefinedConstant] {
    import Type._
    val values: immutable.IndexedSeq[PredefinedConstant] = findValues
    val functions0: List[PredefinedConstant] = List(Empty, PI)
    val nonFunctions0: List[PredefinedConstant] = values.toList.filter(!functions0.contains(_))

    // Constants
    case object Point extends PredefinedConstant(Dbl =>: Dbl =>: Type.Point, Nil)
    case object Floor extends PredefinedConstant(Dbl =>: Integer, Nil)
    case object ToDbl extends PredefinedConstant(Integer =>: Dbl, Nil)
    case object X extends PredefinedConstant(Type.Point =>: Dbl, Nil)
    case object Y extends PredefinedConstant(Type.Point =>: Dbl, Nil)
    case object Add
        extends PredefinedConstant(Var("T") =>: Var("T") =>: Var("T"), List(Predicate("Semigroup", List(Var("T")))))
    case object Div extends PredefinedConstant(Dbl =>: Dbl =>: Dbl, Nil)
    case object Exp extends PredefinedConstant(Dbl =>: Dbl =>: Dbl, Nil)
    case object Abs extends PredefinedConstant(Dbl =>: Dbl, Nil)
    case object Sign extends PredefinedConstant(Dbl =>: Dbl, Nil)
    case object Inverse extends PredefinedConstant(Var("T") =>: Var("T"), Nil)
    case object Multiply extends PredefinedConstant(Dbl =>: Var("T") =>: Var("T"), Nil)
    case object Sin extends PredefinedConstant(Dbl =>: Dbl, Nil)
    case object Cos extends PredefinedConstant(Dbl =>: Dbl, Nil)
    case object Eq extends PredefinedConstant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object If extends PredefinedConstant(Bool =>: Var("T") =>: Var("T") =>: Var("T"), Nil)
    case object PI extends PredefinedConstant(Dbl, Nil)
    case object Mod extends PredefinedConstant(Dbl =>: Dbl =>: Dbl, Nil)

    // Chain
    case object Empty extends PredefinedConstant(Var("T"), Nil)
    case object Cons extends PredefinedConstant(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object MapEmpty extends PredefinedConstant(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object MapCons
        extends PredefinedConstant(
          Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T1")) =>: Evo(Var("T2"))) =>: Evo(Var("T2")),
          Nil)

    // Derived
    case object Polar extends PredefinedConstant(Dbl =>: Dbl =>: Type.Point, Nil)
    case object Constant extends PredefinedConstant(Var("T") =>: Evo(Var("T")), Nil)
    case object Integrate extends PredefinedConstant(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Solve1 extends PredefinedConstant(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")), Nil)
    case object Solve2
        extends PredefinedConstant(
          Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")),
          Nil)
    case object Concat extends PredefinedConstant(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Map extends PredefinedConstant(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")), Nil)
    case object FlatMap
        extends PredefinedConstant(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")), Nil)
    case object Take extends PredefinedConstant(Integer =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object ZipWith
        extends PredefinedConstant(
          Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3")),
          Nil)

    // Distribution
    case object Uniform extends PredefinedConstant(Dbl =>: Dbl =>: Evo(Dbl), Nil)
    case object UniformDiscrete extends PredefinedConstant(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl), Nil)
    case object UniformChoice extends PredefinedConstant(Lst(Var("T")) =>: Evo(Var("T")), Nil)
    case object Normal extends PredefinedConstant(Dbl =>: Dbl =>: Evo(Dbl), Nil)

    // Special functions
    case object Lift extends PredefinedConstant(Var("T"), Nil)

    // functions-only
    case object Fix extends PredefinedFunction((Var("T") =>: Var("T")) =>: Var("T"))
  }

  sealed trait Type {
    type Out
    def children: List[Type] = this match {
      case Type.Evo(inner)      => List(inner)
      case Type.Lst(inner)      => List(inner)
      case Type.Arrow(from, to) => List(from, to)
      case _                    => Nil
    }

    def =>:(from: Type): Type = Type.Arrow(from, this)
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
    final case class Lst(inner: Type) extends Type { type Out = List[inner.type] }
    final case class Arrow(from: Type, to: Type) extends Type { type Out = from.type => to.type }

    // TODO can we do better thant this?
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
        case _            => Left(s"Unable to find an eq typeclass for type $t")
      }
    }.asInstanceOf[Either[String, Eq[t.Out]]]

    def unwrapF(t: Type): Either[String, Type] = t match {
      case Type.Evo(inner) => Right(inner)
      case _               => Left(s"Type $t is not an Evolution type")
    }
  }

  def lift(tpe: Type): Type = tpe match {
    case Type.Arrow(from, to) => lift(from) =>: lift(to)
    case _                    => Type.Evo(tpe)
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

  object TypeClasses {
    case class Predicate(id: String, types: List[Type])
    case class Qualified[T](predicates: List[Predicate], t: T)
    type Instance = Qualified[Predicate]
    object Instance {
      def apply(predicates: List[Predicate], predicate: Predicate): Instance = Qualified(predicates, predicate)
    }
    case class ClassDef(instances: List[Instance])

    // example: Int is Ord, Dbl is Ord, Dbl Ord => Point Ord
    ClassDef(
      List(
        Instance(Nil, Predicate("Ord", List(Type.Integer))),
        Instance(Nil, Predicate("Ord", List(Type.Dbl))),
        Instance(List(Predicate("Ord", List(Type.Dbl))), Predicate("Ord", List(Type.Point)))
      ))
  }
}

trait WithAst[F[_]] {
  val ast: ASTModule[F] = new ASTModule
}
