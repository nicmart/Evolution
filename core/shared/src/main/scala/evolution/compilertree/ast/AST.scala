package evolution.compilertree.ast

import evolution.compilertree.phases.typing.config.{ Constant, Constant0, Constant2 }
import scala.collection.immutable.Nil
import cats.Traverse
import cats.Applicative
import cats.Eval
import cats.Monad
import cats.Functor
import evolution.compilertree.types.TypeClasses.Qualified
import evolution.compilertree.types.Type

sealed trait TreeF[+T]

object TreeF {
  sealed abstract case class Identifier(name: String, primitive: Boolean = false) extends TreeF[Nothing]

  object Identifier {
    def apply(name: String, primitive: Boolean = false): Identifier = new Identifier(name.toLowerCase, primitive) {}

  }

  final case class Lambda[T](varName: String, expr: T) extends TreeF[T]
  final case class App[T](f: T, args: T) extends TreeF[T]
  final case class Let[T](varName: String, expr: T, in: T) extends TreeF[T]
  final case class DoubleLiteral(n: Double) extends TreeF[Nothing]
  final case class IntLiteral(n: Int) extends TreeF[Nothing]
  final case class Bool(b: Boolean) extends TreeF[Nothing]
  final case class Lst[T](ts: List[T]) extends TreeF[T]

  final case class Tree(value: TreeF[Tree])
  final case class CoTree[A](value: A, tail: TreeF[CoTree[A]])

  final type TypedTree = CoTree[Qualified[Type]]

  implicit class TreeFOps(tree: TreeF[Tree]) {
    def embed: Tree = Tree(tree)
  }

  implicit class CoTreeOps[A](tree: TreeF[CoTree[A]]) {
    def annotate(a: A): CoTree[A] = CoTree(a, tree)
  }

  def AppN(f: Tree, args: Tree*): Tree = args.toList match {
    case Nil                => f
    case argHead :: argTail => AppN(TreeF.App(f, argHead).embed, argTail: _*)
  }

  def Const(constant: Constant): Tree = TreeF.Identifier(constant.entryName).embed

  def ConsN(asts: List[Tree]): Tree = asts match {
    case Nil          => TreeF.Identifier(Constant0.Empty.entryName).embed
    case head :: tail => AppN(Const(Constant2.Cons), head, ConsN(tail))
  }

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
    case Let(id, expr, in)  => ppFunc("Let", List(id, expr, in))
    case Lambda(id, expr)   => ppFunc("Lambda", List(id, expr))
    case id: Identifier     => id.name
    case DoubleLiteral(d)   => d.toString
    case Bool(b)            => b.toString
    case IntLiteral(n)      => n.toString
    case Lst(ts)            => ppFunc("Lst", ts)
  }

  def pprintTree(tree: Tree): String = cata(pprintTreeF)(tree)

  def cata[A](f: TreeF[A] => A)(tree: Tree): A =
    f(tree.value.map(cata(f)))

  def cataM[A, M[_]: Monad](f: TreeF[A] => M[A])(tree: Tree): M[A] =
    tree.value.traverse(cataM(f)).flatMap(f)

  def attrM[A, M[_]: Functor](f: TreeF[A] => M[A])(cotree: TreeF[CoTree[A]]): M[CoTree[A]] =
    f(cotree.map(_.value)).map(a => CoTree(a, cotree))

  def ana[A](f: A => TreeF[A])(a: A): Tree =
    Tree(f(a).map(ana(f)))

  def anaM[A, M[_]: Monad](f: A => M[TreeF[A]])(a: A): M[Tree] =
    f(a).flatMap(_.traverse(anaM(f))).map(Tree)

  private def ppFunc(name: String, children: List[String]): String =
    children.mkString(s"$name(", ",", ")")
}
