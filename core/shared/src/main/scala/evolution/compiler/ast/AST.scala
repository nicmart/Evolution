package evolution.compiler.ast

import evolution.compiler.phases.typing.config.{ Constant, Constant0, Constant2, Constant3 }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import scala.collection.immutable.Nil
import evolution.compiler.phases.typing.config.Constant1

sealed trait AST {
  val tpe: Qualified[Type]
  final type Out = tpe.t.Out

  final def withType(tpe: Qualified[Type]): AST = this match {
    case AST.Identifier(name, _, isPrimitive) => AST.Identifier(name, tpe, isPrimitive)
    case AST.App(f, x, _)                     => AST.App(f, x, tpe)
    case AST.Lambda(varName, expr, _)         => AST.Lambda(varName, expr, tpe)
    case AST.Let(varName, expr, in, _)        => AST.Let(varName, expr, in, tpe)
    case AST.DoubleLiteral(n, _)              => AST.DoubleLiteral(n, tpe)
    case AST.IntLiteral(n, _)                 => AST.IntLiteral(n, tpe)
    case AST.Bool(b, _)                       => AST.Bool(b, tpe)
    case AST.Lst(ts, _)                       => AST.Lst(ts, tpe)
  }

  final def withType(tpe: Type): AST = withType(Qualified(tpe))

  final def children: List[AST] = this match {
    case AST.Lambda(_, expr, _)  => List(expr)
    case AST.Let(_, expr, in, _) => List(expr, in)
    case AST.App(f, x, _)        => List(f, x)
    case _                       => Nil
  }
}

object AST {

  sealed abstract case class Identifier(
    name: String,
    tpe: Qualified[Type] = Qualified(Type.Var("")),
    primitive: Boolean = false
  ) extends AST

  object Identifier {
    def apply(name: String, tpe: Qualified[Type] = Qualified(Type.Var("")), primitive: Boolean = false): Identifier =
      new Identifier(name.toLowerCase, tpe, primitive) {}
  }

  final case class Lambda(varName: Identifier, expr: AST, tpe: Qualified[Type]) extends AST
  final case class App(f: AST, args: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Let(varName: Identifier, expr: AST, in: AST, tpe: Qualified[Type]) extends AST
  final case class DoubleLiteral(n: Double, tpe: Qualified[Type] = Qualified(Type.Dbl)) extends AST
  final case class IntLiteral(n: Int, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Bool(b: Boolean, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Lst(ts: List[AST], tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST

  object Lambda {
    def apply(varNameS: String, expr: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))): Lambda =
      Lambda(Identifier(varNameS), expr, tpe)
  }

  object Let {
    def apply(varNameS: String, expr: AST, in: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))): Let =
      Let(Identifier(varNameS), expr, in, tpe)
  }

  def Const(constant: Constant): AST = AST.Identifier(constant.entryName)
  def PrimitiveConst(constant: Constant): AST = AST.Identifier(constant.entryName, primitive = true)

  def AppN(f: AST, args: AST*): AST = args.toList match {
    case Nil                => f
    case argHead :: argTail => AppN(AST.App(f, argHead), argTail: _*)
  }

  def ConsN(asts: List[AST]): AST = asts match {
    case Nil          => AST.Identifier(Constant0.Empty.entryName)
    case head :: tail => AST.AppN(AST.Identifier(Constant2.Cons.entryName), head, ConsN(tail))
  }

  object SpecialSyntax {
    def zip(bindings: List[(String, AST)], body: AST): AST = bindings match {
      case h1 :: h2 :: tl =>
        variadicZipWith(buildLambda(bindings.map(_._1), body), h1._2, h2._2, tl.map(_._2))
      case h1 :: Nil => AST.AppN(AST.Const(Constant2.Map), h1._2, buildLambda(List(h1._1), body))
      case Nil       => body
    }

    def withFirst(binding: (String, AST), body: AST): AST =
      AST.AppN(AST.Const(Constant2.WithFirst), binding._2, buildLambda(List(binding._1), body))

    def product(bindings: List[(String, AST)], body: AST): AST = bindings match {
      case Nil       => body
      case h1 :: Nil => AST.AppN(AST.Const(Constant2.Map), h1._2, buildLambda(List(h1._1), body))
      case h1 :: tail =>
        AST.AppN(AST.Const(Constant2.FlatMap), h1._2, buildLambda(List(h1._1), product(tail, body)))
    }

    def functionBinding(name: String, args: List[String], value: AST, body: AST): AST =
      AST.Let(name, buildLambda(args, value), body)

    def uniformChoice(args: List[AST]): AST =
      AST.App(AST.Const(Constant1.UniformChoice), AST.Lst(args))

    private def buildLambda(vars: List[String], body: AST): AST =
      vars match {
        case Nil          => body
        case head :: tail => Lambda(head, buildLambda(tail, body))
      }

    private def variadicZipWith(f: AST, arg1: AST, arg2: AST, rest: List[AST]): AST =
      rest match {
        case Nil => AppN(AST.Const(Constant3.ZipWith), arg1, arg2, f)
        case nonEmptyRest =>
          AppN(
            AST.Const(Constant3.ZipWith),
            variadicZipWith(f, arg1, arg2, nonEmptyRest.dropRight(1)),
            nonEmptyRest.last,
            AST.Lambda("f", AST.Lambda("x", AST.App(AST.Identifier("f"), AST.Identifier("x"))))
          )
      }
  }

  // Note: f is not (and can't) be applied to variables
  def transformChildren(tree: AST, f: AST => AST): AST = tree match {
    case Lambda(varName, expr, tpe)  => Lambda(varName, f(expr), tpe)
    case Let(varName, expr, in, tpe) => Let(varName, f(expr), f(in), tpe)
    case App(g, x, tpe)              => App(f(g), f(x), tpe)
    case Lst(ts, tpe)                => Lst(ts.map(f), tpe)
    case _                           => tree
  }

  def transformRecursively(tree: AST, f: AST => AST): AST =
    f(transformChildren(tree, transformRecursively(_, f)))

  def prettyPrint(ast: AST): String = {
    def pp(level: Int)(ast: AST): String = {
      val indent = " " * (level * 4)
      ast match {
        case Lambda(varName, expr, tpe) =>
          ppFunc(level, "Lambda", List(varName, expr), tpe.t)
        case id: Identifier => s"${indent}${id.name}: ${id.tpe.predicates} => ${id.tpe.t}"
        case Let(varName, expr, in, tpe) =>
          ppFunc(level, "Let", List(varName, expr, in), tpe.t)
        case IntLiteral(n, tpe) =>
          s"${indent}$n: ${tpe.t}"
        case Bool(b, tpe)          => s"${indent}$b: ${tpe.t}"
        case DoubleLiteral(n, tpe) => s"${indent}$n: ${tpe.t}"
        case App(f, args, tpe)     => ppFunc(level, "App", List(f, args), tpe.t)
        case Lst(ts, tpe)          => ppFunc(level, "Lst", ts, tpe.t)
      }
    }

    def ppFunc(level: Int, name: String, children: List[AST], tpe: Type): String = {
      val indent = " " * (level * 4)
      children.map(pp(level + 1)).mkString(s"${indent}$name(\n", ",\n", s"\n${indent}): $tpe")
    }

    pp(0)(ast)
  }

}
