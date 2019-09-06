package evolution.compilertree.ast

import evolution.compilertree.phases.typing.config.{ Constant, Constant0, Constant2 }
import scala.collection.immutable.Nil
import cats.Traverse
import cats.Applicative
import cats.Eval
import cats.Monad
import cats.Functor
import cats.implicits._
import evolution.compilertree.types.TypeClasses.Qualified
import evolution.compilertree.types.Type
import cats.data.NonEmptyList

sealed trait TreeF[+T]

object TreeF {
  sealed abstract case class Identifier(name: String, primitive: Boolean = false) extends TreeF[Nothing]

  object Identifier {
    def apply(name: String, primitive: Boolean = false): Identifier = new Identifier(name.toLowerCase, primitive) {}

  }

  final case class Lambda[T](varName: String, expr: T) extends TreeF[T]
  final case class App[T](f: T, args: NonEmptyList[T]) extends TreeF[T]
  final case class Let[T](varName: String, expr: T, in: T) extends TreeF[T]
  final case class DoubleLiteral(n: Double) extends TreeF[Nothing]
  final case class IntLiteral(n: Int) extends TreeF[Nothing]
  final case class Bool(b: Boolean) extends TreeF[Nothing]
  final case class Lst[T](ts: List[T]) extends TreeF[T]

  final case class Tree(value: TreeF[Tree])
  final case class CoTree[A](value: A, tail: TreeF[CoTree[A]])

  final type TypedTree = CoTree[Qualified[Type]]

  implicit class TreeFTreeOps(tree: TreeF[Tree]) {
    def embed: Tree = Tree(tree)
  }

  implicit class TreeFOps[A](tree: TreeF[A]) {
    def children: List[A] = tree
      .foldRight[List[A]](Eval.now(Nil)) { (a, evalAs) =>
        evalAs.map(a :: _)
      }
      .value
  }

  implicit class CoTreeOps[A](tree: TreeF[CoTree[A]]) {
    def annotate(a: A): CoTree[A] = CoTree(a, tree)
  }

  def AppN(f: Tree, argHead: Tree, argTail: Tree*): Tree =
    App(f, NonEmptyList(argHead, argTail.toList)).embed

  def Const(constant: Constant): Tree = TreeF.Identifier(constant.entryName).embed
  def TypedConst(constant: Constant, tpe: Qualified[Type]): TypedTree =
    CoTree(tpe, TreeF.Identifier(constant.entryName))
  def PrimitiveConst(constant: Constant): Tree = TreeF.Identifier(constant.entryName, primitive = true).embed
  def TypedPrimitiveConst(constant: Constant, tpe: Qualified[Type]): TypedTree =
    CoTree(tpe, TreeF.Identifier(constant.entryName, primitive = true))

  def ConsN(asts: List[Tree]): Tree = asts match {
    case Nil          => TreeF.Identifier(Constant0.Empty.entryName).embed
    case head :: tail => AppN(Const(Constant2.Cons), head, ConsN(tail))
  }

  implicit val traverseForTreeF: Traverse[TreeF] = new Traverse[TreeF] {
    def traverse[G[_]: Applicative, A, B](fa: TreeF[A])(f: A => G[B]): G[TreeF[B]] =
      fa match {
        case TreeF.App(g, args)          => (f(g), args.traverse(f)).mapN(TreeF.App[B])
        case Lambda(varName, expr)       => f(expr).map(Lambda(varName, _))
        case Lst(ts)                     => ts.traverse(f).map(Lst[B])
        case Let(varName, expr, in)      => (f(expr), f(in)).mapN(Let(varName, _, _))
        case Identifier(name, primitive) => Identifier(name, primitive).pure[G].widen
        case DoubleLiteral(n)            => DoubleLiteral(n).pure[G].widen
        case Bool(b)                     => Bool(b).pure[G].widen
        case IntLiteral(n)               => IntLiteral(n).pure[G].widen
      }

    def foldLeft[A, B](fa: TreeF[A], z: B)(f: (B, A) => B): B = fa match {

      case TreeF.App(g, args) => args.foldLeft(f(z, g))(f)
      case Let(_, expr, in)   => f(f(z, expr), in)
      case Lambda(_, expr)    => f(z, expr)
      case _: Identifier      => z
      case DoubleLiteral(_)   => z
      case Bool(_)            => z
      case IntLiteral(_)      => z
      case Lst(ts)            => ts.foldLeft(z)(f)
    }

    def foldRight[A, B](fa: TreeF[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = fa match {

      case TreeF.App(g, args) => f(g, args.foldRight(z)(f))
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
    case TreeF.App(g, args) => ppFunc("App", g :: args.toList)
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

  def cataCoTree[A, B](f: (B, TreeF[A]) => A)(tree: CoTree[B]): A =
    f(tree.value, tree.tail.map(cataCoTree(f)))

  def cataM[A, M[_]: Monad](f: TreeF[A] => M[A])(tree: Tree): M[A] =
    tree.value.traverse(cataM(f)).flatMap(f)

  def cataMCoTree[A, B, M[_]: Monad](f: (B, TreeF[A]) => M[A])(tree: CoTree[B]): M[A] =
    tree.tail.traverse(cataMCoTree(f)).flatMap(tfa => f(tree.value, tfa))

  def attr[A](f: TreeF[A] => A)(tree: Tree): CoTree[A] = {
    val x = tree.value.map(attr(f))
    CoTree(cata(f)(tree), x)
  }

  def attrM[A, M[_]: Functor](f: TreeF[A] => M[A])(cotree: TreeF[CoTree[A]]): M[CoTree[A]] =
    f(cotree.map(_.value)).map(a => CoTree(a, cotree))

  def ana[A](f: A => TreeF[A])(a: A): Tree =
    Tree(f(a).map(ana(f)))

  def anaM[A, M[_]: Monad](f: A => M[TreeF[A]])(a: A): M[Tree] =
    f(a).flatMap(_.traverse(anaM(f))).map(Tree)

  def histoCo[A, B](f: (B, TreeF[CoTree[A]]) => A)(tree: CoTree[B]): A = ???
  //f(tree.value, tree.tail.map(histo(f)))

  implicit class CoOps[A](coTree: CoTree[A]) {
    def cojoin: CoTree[CoTree[A]] = CoTree(coTree, coTree.tail.map(_.cojoin))
    def map[B](f: A => B): CoTree[B] = CoTree(f(coTree.value), coTree.tail.map(_.map(f)))
  }

  def unfold[A, B](b: B)(f: B => (A, TreeF[B])): CoTree[A] = {
    val (a, fb) = f(b)
    CoTree(a, fb.map(unfold(_)(f)))
  }

  // Note: ∘ is just map!
  // Cofree.unfold(m)(as => (as ∘ (_.copure), k(as ∘ (_.tail)))) ?
  def dist[A](fca0: TreeF[CoTree[A]]): CoTree[TreeF[A]] =
    unfold[TreeF[A], TreeF[CoTree[A]]](fca0)(fca => (fca.map(_.value), fca.map(_.tail)))

  def histo[A](f: TreeF[CoTree[A]] => A)(tree: Tree): A =
    cata[CoTree[A]](fca => dist(fca.map(_.cojoin)).map(f))(tree).value
  // cata[W[A]](t)(fwa => k(fwa.map(_.cojoin)).map(g)).copoint
  // k: TreeF[CoTree[?]] =? CoTree[TreeF[?]]
  // tree.value // TreeF[Tree]
  // tree.value.map(tree => histo(f)(tree), ) // TreeF[A]

  private def ppFunc(name: String, children: List[String]): String =
    children.mkString(s"$name(", ",", ")")
}
