package evolution.language

import enumeratum.EnumEntry.Lowercase
import enumeratum.{ Enum, EnumEntry }

import scala.collection.immutable

trait ASTModule[F[_]] { self: TypesModule[F] with PredefinedConstantsModule[F] =>
  import TypeClasses._

  sealed trait AST {
    val tpe: Qualified[Type]
    final type Out = tpe.t.Out

    def withType(tpe: Qualified[Type]): AST = this match {
      case AST.Identifier(name, _, isPrimitive) => AST.Identifier(name, tpe, isPrimitive)
      case AST.App(f, x, _)                     => AST.App(f, x, tpe)
      case AST.Lambda(varName, expr, _)         => AST.Lambda(varName, expr, tpe)
      case AST.Let(varName, expr, in, _)        => AST.Let(varName, expr, in, tpe)
      case AST.Number(n, _)                     => AST.Number(n, tpe)
      case AST.Bool(b, _)                       => AST.Bool(b, tpe)
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

    sealed abstract case class Identifier(
      name: String,
      tpe: Qualified[Type] = Qualified(Type.Var("")),
      primitive: Boolean = false
    ) extends AST

    // TODO Either we lowercase all the identifiers (i.e. also let and lambda names) or we do not do it at all.
    // TODO: Note, we could use an Identifier case class that does the job, and it will be shared between lambda and let

    object Identifier {
      def apply(name: String, tpe: Qualified[Type] = Qualified(Type.Var("")), primitive: Boolean = false): Identifier =
        new Identifier(name.toLowerCase, tpe, primitive) {}
    }

    final case class Lambda(varName: String, expr: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
    final case class App(f: AST, x: AST, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
    final case class Let(varName: String, expr: AST, in: AST, tpe: Qualified[Type] = Qualified(Type.Var("")))
        extends AST
    final case class Number(n: String, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST
    final case class Bool(b: Boolean, tpe: Qualified[Type] = Qualified(Type.Var(""))) extends AST

    def Const(constant: Constant): AST = AST.Identifier(constant.entryName)
    def PrimitiveConst(constant: Constant): AST = AST.Identifier(constant.entryName, primitive = true)
    def Lift(ast: AST): AST = App(Identifier(Constant1.Lift.entryName), ast)
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
}
