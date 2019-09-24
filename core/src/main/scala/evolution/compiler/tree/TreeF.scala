package evolution.compiler.tree

import cats.{ Applicative, Eval, Traverse }
import cats.data.NonEmptyList
import cats.implicits._
import evolution.compiler.phases.typing.config.Constant

import cats.data.Const
import cats.Foldable

sealed trait TreeF[+T]

object TreeF {

  sealed abstract case class Identifier(name: String, primitive: Boolean = false) extends TreeF[Nothing]
  final case class Lambda[T](varName: String, expr: T) extends TreeF[T]
  final case class App[T](f: T, args: NonEmptyList[T]) extends TreeF[T]
  final case class Let[T](varName: String, expr: T, in: T) extends TreeF[T]
  final case class DoubleLiteral(n: Double) extends TreeF[Nothing]
  final case class IntLiteral(n: Int) extends TreeF[Nothing]
  final case class Bool(b: Boolean) extends TreeF[Nothing]
  final case class Lst[T](ts: List[T]) extends TreeF[T]

  object Identifier {
    def apply(name: String, primitive: Boolean = false): Identifier = new Identifier(name.toLowerCase, primitive) {}
    def const[T](constant: Constant): TreeF[T] = TreeF.Identifier(constant.entryName, primitive = false)
    def primitiveConst[T](constant: Constant): TreeF[T] = TreeF.Identifier(constant.entryName, primitive = true)
  }

  object App {
    def of[T](f: T, arg1: T, args: T*): TreeF[T] = App(f, NonEmptyList(arg1, args.toList))
  }

  implicit class TreeOps(tree: TreeF[Tree]) {
    def embed: Tree = Tree(tree)
  }

  implicit class Ops[A](fa: TreeF[A]) {
    def children: List[A] = fa.traverse[Const[List[A], ?], Nothing](a => Const(List(a))).getConst
  }

  implicit class AnnotatedOps[A](tree: TreeF[AnnotatedTree[A]]) {
    def annotate(a: A): AnnotatedTree[A] = AnnotatedTree(a, tree)
  }

  implicit val traverseForTreeF: Traverse[TreeF] = new Traverse[TreeF] {
    def traverse[G[_]: Applicative, A, B](fa: TreeF[A])(f: A => G[B]): G[TreeF[B]] =
      fa match {
        case TreeF.App(g, args)          => (f(g), args.traverse(f)).mapN(TreeF.App[B] _)
        case Lambda(varName, expr)       => f(expr).map(Lambda(varName, _))
        case Lst(ts)                     => ts.traverse(f).map(Lst[B])
        case Let(varName, expr, in)      => (f(expr), f(in)).mapN(Let(varName, _, _))
        case Identifier(name, primitive) => Identifier(name, primitive).pure[G].widen
        case DoubleLiteral(n)            => DoubleLiteral(n).pure[G].widen
        case Bool(b)                     => Bool(b).pure[G].widen
        case IntLiteral(n)               => IntLiteral(n).pure[G].widen
      }

    def foldLeft[A, B](fa: TreeF[A], z: B)(f: (B, A) => B): B =
      Foldable[List].foldLeft(fa.children, z)(f)

    def foldRight[A, B](fa: TreeF[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[List].foldRight(fa.children, z)(f)
  }
}
