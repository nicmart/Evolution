package evolution.language

import enumeratum.{ Enum, EnumEntry }
import scala.collection.immutable

trait ASTModule[F[_]] extends TypesModule[F] {
  import TypeClasses._

  sealed trait AST {
    val tpe: Qualified[Type]
    final type Out = tpe.t.Out

    def withType(tpe: Qualified[Type]): AST = this match {
      case AST.Identifier(name, _, _)    => AST.Identifier(name, tpe)
      case AST.Const(id, _, ps)          => AST.Const(id, tpe, ps)
      case AST.App(f, x, _)              => AST.App(f, x, tpe)
      case AST.Lambda(varName, expr, _)  => AST.Lambda(varName, expr, tpe)
      case AST.Let(varName, expr, in, _) => AST.Let(varName, expr, in, tpe)
      case AST.Number(n, _)              => AST.Number(n, tpe)
      case AST.Bool(b, _)                => AST.Bool(b, tpe)
    }

    def withType(tpe: Type): AST = withType(Qualified(tpe))

    def children: List[AST] = this match {
      case AST.Lambda(varName, expr, _)  => List(expr)
      case AST.Let(varName, expr, in, _) => List(expr, in)
      case AST.App(f, x, _)              => List(f, x)
      case _                             => Nil
    }
  }

  object AST {
    final case class Identifier(
      name: String,
      tpe: Qualified[Type] = Qualified(Type.Var("")),
      primitive: Boolean = false)
        extends AST
    final case class Lambda(varName: String, expr: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
    final case class App(f: AST, x: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
    final case class Const(
      id: Constant,
      tpe: Qualified[Type] = Qualified(Type.Var("")),
      predicates: List[Predicate] = Nil)
        extends AST
    final case class Let(varName: String, expr: AST, in: AST, tpe: Qualified[Type] = Qualified(Type.Var("")))
        extends AST
    final case class Number(n: String, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
    final case class Bool(b: Boolean, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST

    def Lift(ast: AST): AST = App(Const(Constant.Lift), ast)
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

  abstract sealed class Constant(val scheme: Type, val predicates: List[Predicate]) extends EnumEntry

  object Constant extends Enum[Constant] {
    import Type._
    val values: immutable.IndexedSeq[Constant] = findValues
    val functions0: List[Constant] = List(Empty, PI)
    val nonFunctions0: List[Constant] = values.toList.filter(!functions0.contains(_))

    // Constants

    // Math
    case object Point extends Constant(Dbl =>: Dbl =>: Type.Point, Nil)
    case object Floor extends Constant(Dbl =>: Integer, Nil)
    case object ToDbl extends Constant(Integer =>: Dbl, Nil)
    case object X extends Constant(Type.Point =>: Dbl, Nil)
    case object Y extends Constant(Type.Point =>: Dbl, Nil)
    case object Add extends Constant(Var("T") =>: Var("T") =>: Var("T"), List(Predicate("Semigroup", List(Var("T")))))
    case object Minus extends Constant(Var("T") =>: Var("T") =>: Var("T"), List(Predicate("Semigroup", List(Var("T")))))
    case object Div extends Constant(Dbl =>: Dbl =>: Dbl, Nil)
    case object Exp extends Constant(Dbl =>: Dbl =>: Dbl, Nil)
    case object Abs extends Constant(Dbl =>: Dbl, Nil)
    case object Sign extends Constant(Dbl =>: Dbl, Nil)
    case object Inverse extends Constant(Var("T") =>: Var("T"), Nil)
    case object Multiply extends Constant(Dbl =>: Var("T") =>: Var("T"), Nil)
    case object Sin extends Constant(Dbl =>: Dbl, Nil)
    case object Cos extends Constant(Dbl =>: Dbl, Nil)
    case object PI extends Constant(Dbl, Nil)
    case object Mod extends Constant(Dbl =>: Dbl =>: Dbl, Nil)

    // Boolean
    case object If extends Constant(Bool =>: Var("T") =>: Var("T") =>: Var("T"), Nil)
    case object Not extends Constant(Bool =>: Bool, Nil)
    case object And extends Constant(Bool =>: Bool =>: Bool, Nil)
    case object Or extends Constant(Bool =>: Bool =>: Bool, Nil)
    case object Eq extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object Neq extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object GreaterThan extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object GreaterThanOrEqual extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object LessThan extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)
    case object LessThanOrEqual extends Constant(Var("T") =>: Var("T") =>: Bool, Nil)

    // Chain
    case object Empty extends Constant(Var("T"), Nil)
    case object Cons extends Constant(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object MapEmpty extends Constant(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object MapCons
        extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T1")) =>: Evo(Var("T2"))) =>: Evo(Var("T2")), Nil)

    // Derived
    case object Polar extends Constant(Dbl =>: Dbl =>: Type.Point, Nil)
    case object Constant extends Constant(Var("T") =>: Evo(Var("T")), Nil)
    case object Integrate extends Constant(Var("T") =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Solve1 extends Constant(Evo(Var("T") =>: Var("T")) =>: Var("T") =>: Evo(Var("T")), Nil)
    case object Solve2
        extends Constant(Evo(Var("T") =>: Var("T") =>: Var("T")) =>: Var("T") =>: Var("T") =>: Evo(Var("T")), Nil)
    case object Concat extends Constant(Evo(Var("T")) =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Map extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Var("T2")) =>: Evo(Var("T2")), Nil)
    case object FlatMap extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Evo(Var("T2"))) =>: Evo(Var("T2")), Nil)
    case object Take extends Constant(Integer =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object ZipWith
        extends Constant(
          Evo(Var("T1")) =>: Evo(Var("T2")) =>: (Var("T1") =>: Var("T2") =>: Var("T3")) =>: Evo(Var("T3")),
          Nil)
    case object While extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1")), Nil)
    case object Until extends Constant(Evo(Var("T1")) =>: (Var("T1") =>: Bool) =>: Evo(Var("T1")), Nil)
    case object InRect extends Constant(Type.Point =>: Type.Point =>: Type.Point =>: Bool, Nil)

    // Distribution
    case object Uniform extends Constant(Dbl =>: Dbl =>: Evo(Dbl), Nil)
    case object UniformDiscrete extends Constant(Dbl =>: Dbl =>: Dbl =>: Evo(Dbl), Nil)
    case object UniformFrom extends Constant(Integer =>: Evo(Var("T")) =>: Evo(Var("T")), Nil)
    case object Normal extends Constant(Dbl =>: Dbl =>: Evo(Dbl), Nil)

    // Special functions
    case object Lift extends Constant(Var("T"), Nil)

    // functions-only
    case object Fix extends Constant((Var("T") =>: Var("T")) =>: Var("T"), Nil)
  }
}
