package evolution.compiler.tree

import cats.data.{Const, NonEmptyList}
import cats.implicits.*
import cats.{Applicative, Eval, Foldable, Traverse}
import evolution.compiler.tree.TreeF.CaseInsensitiveName

enum TreeF[+T](val pos: Pos):
  case Id(name: CaseInsensitiveName, override val pos: Pos = NoPos) extends TreeF[Nothing](pos)
  case DoubleLiteral(n: Double, override val pos: Pos = NoPos) extends TreeF[Nothing](pos)
  case IntLiteral(n: Int, override val pos: Pos = NoPos) extends TreeF[Nothing](pos)
  case Bool(b: Boolean, override val pos: Pos = NoPos) extends TreeF[Nothing](pos)
  case Lambda(varName: String, expr: T, override val pos: Pos = NoPos) extends TreeF[T](pos)
  case App(f: T, args: NonEmptyList[T], override val pos: Pos = NoPos) extends TreeF[T](pos)
  case Let(varName: String, expr: T, in: T, override val pos: Pos = NoPos) extends TreeF[T](pos)
  case Lst(ts: List[T], override val pos: Pos = NoPos) extends TreeF[T](pos)

object TreeF:
  extension [T](tree: TreeF[T])
    def withPos(pos: Pos): TreeF[T] = tree match
      case tree: Id => tree.copy(pos = pos)
      case tree: DoubleLiteral => tree.copy(pos = pos)
      case tree: IntLiteral => tree.copy(pos = pos)
      case tree: Bool => tree.copy(pos = pos)
      case tree: Lambda[T] => tree.copy(pos = pos)
      case tree: App[T] => tree.copy(pos = pos)
      case tree: Let[T] => tree.copy(pos = pos)
      case tree: Lst[T] => tree.copy(pos = pos)
  opaque type CaseInsensitiveName = String

  object CaseInsensitiveName:
    def apply(name: String): CaseInsensitiveName = name.toLowerCase
    extension (name: CaseInsensitiveName) def string: String = name

  object App:
    def of[T](f: T, arg1: T, args: T*): TreeF[T] = App(f, NonEmptyList(arg1, args.toList), NoPos)

  extension (tree: TreeF[Tree]) def embed: Tree = Tree(tree)

  extension [A](fa: TreeF[A])
    def children: List[A] = fa.traverse[Const[List[A], _], Nothing](a => Const(List(a))).getConst

  extension [A](tree: TreeF[AnnotatedTree[A]]) def annotate(a: A): AnnotatedTree[A] = AnnotatedTree(a, tree)

  given Traverse[TreeF] with
    def traverse[G[_]: Applicative, A, B](fa: TreeF[A])(f: A => G[B]): G[TreeF[B]] =
      fa match
        case TreeF.App(g, args, pos)     => (f(g), args.traverse(f), pos.pure[G]).mapN(TreeF.App[B])
        case Lambda(varName, expr, pos)  => f(expr).map(Lambda(varName, _, pos))
        case Lst(ts, pos)                => ts.traverse(f).map(ts => Lst[B](ts, pos))
        case Let(varName, expr, in, pos) => (f(expr), f(in)).mapN(Let(varName, _, _, pos))
        case Id(name, pos)               => Id(name).pure[G].widen
        case DoubleLiteral(n, pos)       => DoubleLiteral(n, pos).pure[G].widen
        case Bool(b, pos)                => Bool(b, pos).pure[G].widen
        case IntLiteral(n, pos)          => IntLiteral(n, pos).pure[G].widen

    def foldLeft[A, B](fa: TreeF[A], z: B)(f: (B, A) => B): B =
      Foldable[List].foldLeft(fa.children, z)(f)

    def foldRight[A, B](fa: TreeF[A], z: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      Foldable[List].foldRight(fa.children, z)(f)
