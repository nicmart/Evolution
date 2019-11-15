package evolution.compiler.tree

import cats.data.NonEmptyList

abstract class TreeBuilder[T, F[_]] {
  def toF: TreeF[T] => F[T]

  final def Id(name: String, primitive: Boolean = false): F[T] = toF(TreeF.Id(name, primitive))
  final def Lambda(varName: String, expr: T): F[T] = toF(TreeF.Lambda(varName, expr))
  final def App(f: T, args: NonEmptyList[T]): F[T] = toF(TreeF.App(f, args))
  final def Let(varName: String, expr: T, in: T): F[T] = toF(TreeF.Let(varName, expr, in))
  final def Lst(ts: List[T]): F[T] = toF(TreeF.Lst(ts))
  final def DoubleLiteral(n: Double): F[T] = toF(TreeF.DoubleLiteral(n))
  final def IntLiteral(n: Int): F[T] = toF(TreeF.IntLiteral(n))
  final def Bool(b: Boolean): F[T] = toF(TreeF.Bool(b))

  private def App2(f: T, args: NonEmptyList[T]): F[T] = App(f, args)

  object App {
    def of(f: T, a: T, others: T*): F[T] = App2(f, NonEmptyList(a, others.toList))
  }
}
