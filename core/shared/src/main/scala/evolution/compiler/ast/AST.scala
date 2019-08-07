package evolution.compiler.ast

import evolution.compiler.phases.typing.config.{ Constant, Constant0, Constant2, Constant3 }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified

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

  final case class Lambda(varName: String, expr: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class App(f: AST, args: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Let(varName: String, expr: AST, in: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class DoubleLiteral(n: Double, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class IntLiteral(n: Int, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Bool(b: Boolean, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST

  def Const(constant: Constant): AST = AST.Identifier(constant.entryName)
  def PrimitiveConst(constant: Constant): AST = AST.Identifier(constant.entryName, primitive = true)

  def AppN(f: AST, args: AST*): AST =
    args.toList match {
      case arg1 :: arg2 :: arg3 :: rest if f == AST.Const(Constant3.ZipWith) =>
        variadicZipWith(args.last, arg1, arg2, (arg3 :: rest).dropRight(1))
      case _ => nestedApp(f, args: _*)
    }

  private def nestedApp(f: AST, args: AST*): AST = args.toList match {
    case Nil                => f
    case argHead :: argTail => nestedApp(AST.App(f, argHead), argTail: _*)
  }

  def ConsN(asts: List[AST]): AST = asts match {
    case Nil          => AST.Identifier(Constant0.Empty.entryName)
    case head :: tail => AST.AppN(AST.Identifier(Constant2.Cons.entryName), head, ConsN(tail))
  }

  private def variadicZipWith(f: AST, arg1: AST, arg2: AST, rest: List[AST]): AST =
    rest match {
      case Nil => nestedApp(AST.Const(Constant3.ZipWith), arg1, arg2, f)
      case nonEmptyRest =>
        nestedApp(
          AST.Const(Constant3.ZipWith),
          variadicZipWith(f, arg1, arg2, nonEmptyRest.dropRight(1)),
          nonEmptyRest.last,
          AST.Lambda("f", AST.Lambda("x", AST.App(AST.Identifier("f"), AST.Identifier("x"))))
        )
    }
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
