package evolution.compiler.ast

import evolution.compiler.phases.typing.config.{ Constant, Constant0, Constant2, Constant3 }
import evolution.compiler.types.Type
import evolution.compiler.types.TypeClasses.Qualified
import scala.collection.immutable.Nil
import evolution.compiler.phases.typing.config.Constant1
import evolution.compiler.types.Typed
import cats.Traverse
import cats.Applicative
import cats.Eval

sealed trait AST {
  val qualifiedType: Qualified[Type]
  final type Out = qualifiedType.value.Out

  final def withType(qualifiedType: Qualified[Type]): AST = this match {
    case AST.Identifier(name, _, isPrimitive) => AST.Identifier(name, qualifiedType, isPrimitive)
    case AST.App(f, x, _)                     => AST.App(f, x, qualifiedType)
    case AST.Lambda(varName, expr, _)         => AST.Lambda(varName, expr, qualifiedType)
    case AST.Let(varName, expr, in, _)        => AST.Let(varName, expr, in, qualifiedType)
    case AST.DoubleLiteral(n, _)              => AST.DoubleLiteral(n, qualifiedType)
    case AST.IntLiteral(n, _)                 => AST.IntLiteral(n, qualifiedType)
    case AST.Bool(b, _)                       => AST.Bool(b, qualifiedType)
    case AST.Lst(ts, _)                       => AST.Lst(ts, qualifiedType)
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
    qualifiedType: Qualified[Type] = Qualified(Type.Var("")),
    primitive: Boolean = false
  ) extends AST

  object Identifier {
    def apply(
      name: String,
      qualifiedType: Qualified[Type] = Qualified(Type.Var("")),
      primitive: Boolean = false
    ): Identifier =
      new Identifier(name.toLowerCase, qualifiedType, primitive) {}
  }

  final case class Lambda(varName: Identifier, expr: AST, qualifiedType: Qualified[Type]) extends AST
  final case class App(f: AST, args: AST, qualifiedType: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Let(varName: Identifier, expr: AST, in: AST, qualifiedType: Qualified[Type]) extends AST
  final case class DoubleLiteral(n: Double, qualifiedType: Qualified[Type] = Qualified(Type.Dbl)) extends AST
  final case class IntLiteral(n: Int, qualifiedType: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Bool(b: Boolean, qualifiedType: Qualified[Type] = Qualified(Type.Var(""))) extends AST
  final case class Lst(ts: List[AST], qualifiedType: Qualified[Type] = Qualified(Type.Var(""))) extends AST

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
        case Lambda(varName, expr, qualifiedType) =>
          ppFunc(level, "Lambda", List(varName, expr), qualifiedType.value)
        case id: Identifier => s"${indent}${id.name}: ${id.qualifiedType.predicates} => ${id.qualifiedType.value}"
        case Let(varName, expr, in, qualifiedType) =>
          ppFunc(level, "Let", List(varName, expr, in), qualifiedType.value)
        case IntLiteral(n, qualifiedType) =>
          s"${indent}$n: ${qualifiedType.value}"
        case Bool(b, qualifiedType)          => s"${indent}$b: ${qualifiedType.value}"
        case DoubleLiteral(n, qualifiedType) => s"${indent}$n: ${qualifiedType.value}"
        case App(f, args, qualifiedType)     => ppFunc(level, "App", List(f, args), qualifiedType.value)
        case Lst(ts, qualifiedType)          => ppFunc(level, "Lst", ts, qualifiedType.value)
      }
    }

    def ppFunc(level: Int, name: String, children: List[AST], tpe: Type): String = {
      val indent = " " * (level * 4)
      children.map(pp(level + 1)).mkString(s"${indent}$name(\n", ",\n", s"\n${indent}): $tpe")
    }

    pp(0)(ast)
  }
}

sealed trait TreeF[+T]

object TreeF {
  sealed abstract case class Identifier(name: String, primitive: Boolean = false) extends TreeF[Nothing]

  object Identifier {
    def apply(name: String, primitive: Boolean = false): Identifier = new Identifier(name.toLowerCase, primitive) {}

  }

  final case class Lambda[T](varName: Identifier, expr: T) extends TreeF[T]
  final case class App[T](f: T, args: T) extends TreeF[T]
  final case class Let[T](varName: Identifier, expr: T, in: T) extends TreeF[T]
  final case class DoubleLiteral(n: Double) extends TreeF[Nothing]
  final case class IntLiteral(n: Int) extends TreeF[Nothing]
  final case class Bool(b: Boolean) extends TreeF[Nothing]
  final case class Lst[T](ts: List[T]) extends TreeF[T]

  final case class Tree(value: TreeF[Tree])
  final case class TypedTree(value: Typed[TreeF[TypedTree]])

  import cats.implicits._
  implicit val traverseForTreeF: Traverse[TreeF] = new Traverse[TreeF] {
    def traverse[G[_]: Applicative, A, B](fa: TreeF[A])(f: A => G[B]): G[TreeF[B]] =
      fa match {
        case TreeF.App(g, args)          => (f(g), f(args)).mapN(TreeF.App[B])
        case Lambda(varName, expr)       => f(expr).map(Lambda(varName, _))
        case Lst(ts)                     => ts.traverse(f).map(Lst[B])
        case Let(varName, expr, in)      => (f(expr), f(in)).mapN(Let(varName, _, _))
        case Identifier(name, primitive) => Identifier(name, primitive).pure[G].widen
        case DoubleLiteral(n)            => DoubleLiteral(n).pure[G].widen
        case Bool(b)                     => Bool(b).pure[G].widen
        case IntLiteral(n)               => IntLiteral(n).pure[G].widen
      }

    def foldLeft[A, B](fa: TreeF[A], z: B)(f: (B, A) => B): B = fa match {

      case TreeF.App(g, args) => f(f(z, g), args)
      case Let(_, expr, in)   => f(f(z, expr), in)
      case Lambda(_, expr)    => f(z, expr)
      case _: Identifier      => z
      case DoubleLiteral(_)   => z
      case Bool(_)            => z
      case IntLiteral(_)      => z
      case Lst(ts)            => ts.foldLeft(z)(f)
    }

    def foldRight[A, B](fa: TreeF[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {

      case TreeF.App(g, args) => f(g, f(args, z))
      case Let(_, expr, in)   => f(expr, f(in, z))
      case Lambda(_, expr)    => f(expr, z)
      case _: Identifier      => z
      case DoubleLiteral(_)   => z
      case Bool(_)            => z
      case IntLiteral(_)      => z
      case Lst(ts)            => ts.foldRight(z)(f)
    }
  }

  def pprintTreeF(treeF: TreeF[String]): String = treeF match {
    case TreeF.App(g, args) => ppFunc("App", List(g, args))
    case Let(id, expr, in)  => ppFunc("Let", List(id.name, expr, in))
    case Lambda(id, expr)   => ppFunc("Lambda", List(id.name, expr))
    case id: Identifier     => id.name
    case DoubleLiteral(d)   => d.toString
    case Bool(b)            => b.toString
    case IntLiteral(n)      => n.toString
    case Lst(ts)            => ppFunc("Lst", ts)
  }

  def pprintTree(tree: Tree): String = cata(pprintTreeF)(tree)

  def cata[A](f: TreeF[A] => A)(tree: Tree): A =
    f(tree.value.map(cata(f)))

  //f: TreeF[TypedTree] => TypedTree 

  def ana[A](f: A => TreeF[A])(a: A): Tree =
    Tree(f(a).map(ana(f)))

  private def ppFunc(name: String, children: List[String]): String =
    children.mkString(s"$name(", ",", ")")
}
