package evolution.compiler.tree

import cats.data.{Const, NonEmptyList}
import cats.implicits.*
import cats.{Applicative, Eval, Foldable, Traverse}
import evolution.compiler.tree.TreeF.CaseInsensitiveName

enum TreeF[+T]:
  case Id(name: CaseInsensitiveName) extends TreeF[Nothing]
  case DoubleLiteral(n: Double) extends TreeF[Nothing]
  case IntLiteral(n: Int) extends TreeF[Nothing]
  case Bool(b: Boolean) extends TreeF[Nothing]
  case Lambda(varName: String, expr: T)
  case App(f: T, args: NonEmptyList[T])
  case Let(varName: String, expr: T, in: T)
  case Lst(ts: List[T])

object TreeF:
  opaque type CaseInsensitiveName = String

  object CaseInsensitiveName:
    def apply(name: String): CaseInsensitiveName = name.toLowerCase
    extension (name: CaseInsensitiveName) def string: String = name

  object App:
    def of[T](f: T, arg1: T, args: T*): TreeF[T] = App(f, NonEmptyList(arg1, args.toList))

  extension (tree: TreeF[Tree]) def embed: Tree = Tree(tree)

  extension [A](fa: TreeF[A])
    def children: List[A] = fa.traverse[Const[List[A], _], Nothing](a => Const(List(a))).getConst

  extension [A](tree: TreeF[AnnotatedTree[A]]) def annotate(a: A): AnnotatedTree[A] = AnnotatedTree(a, tree)

  given Traverse[TreeF] with
    def traverse[G[_]: Applicative, A, B](fa: TreeF[A])(f: A => G[B]): G[TreeF[B]] =
      fa match
        case TreeF.App(g, args)     => (f(g), args.traverse(f)).mapN(TreeF.App[B] _)
        case Lambda(varName, expr)  => f(expr).map(Lambda(varName, _))
        case Lst(ts)                => ts.traverse(f).map(Lst[B])
        case Let(varName, expr, in) => (f(expr), f(in)).mapN(Let(varName, _, _))
        case Id(name)               => Id(name).pure[G].widen
        case DoubleLiteral(n)       => DoubleLiteral(n).pure[G].widen
        case Bool(b)                => Bool(b).pure[G].widen
        case IntLiteral(n)          => IntLiteral(n).pure[G].widen

    def foldLeft[A, B](fa: TreeF[A], z: B)(f: (B, A) => B): B =
      Foldable[List].foldLeft(fa.children, z)(f)

    def foldRight[A, B](fa: TreeF[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[List].foldRight(fa.children, z)(f)
