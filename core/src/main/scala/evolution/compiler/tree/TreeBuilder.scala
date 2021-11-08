package evolution.compiler.tree

import cats.data.NonEmptyList
import evolution.compiler.tree.TreeF.CaseInsensitiveName

abstract class TreeBuilder[T, F[_]]:
  def toF: TreeF[T] => F[T]

  final def Id(name: String, primitive: Boolean = false, pos: Pos = NoPos): F[T] = toF(TreeF.Id(CaseInsensitiveName(name), pos))
  final def Lambda(varName: String, expr: T, pos: Pos = NoPos): F[T] = toF(TreeF.Lambda(varName, expr, pos))
  final def App(f: T, args: NonEmptyList[T], pos: Pos = NoPos): F[T] = toF(TreeF.App(f, args, pos))
  final def Let(varName: String, expr: T, in: T, pos: Pos = NoPos): F[T] = toF(TreeF.Let(varName, expr, in, pos))
  final def Lst(ts: List[T], pos: Pos = NoPos): F[T] = toF(TreeF.Lst(ts, pos))
  final def DoubleLiteral(n: Double, pos: Pos = NoPos): F[T] = toF(TreeF.DoubleLiteral(n, pos))
  final def IntLiteral(n: Int, pos: Pos = NoPos): F[T] = toF(TreeF.IntLiteral(n, pos))
  final def Bool(b: Boolean, pos: Pos = NoPos): F[T] = toF(TreeF.Bool(b, pos))

  private def App2(f: T, args: NonEmptyList[T]): F[T] = App(f, args)

  object App:
    def of(f: T, a: T, others: T*): F[T] = App2(f, NonEmptyList(a, others.toList))
