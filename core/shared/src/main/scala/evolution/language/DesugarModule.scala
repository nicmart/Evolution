package evolution.language
import evolution.data.ExpressionModule

trait DesugarModule[F[_]] { self: ExpressionModule[F] =>
  import Expr._
  object Desugarer {

    def withFirst[T1, T2](expr: Expr[F[T1]], f: Expr[T1 => F[T2]]): Expr[F[T2]] = {
      val (head, tail) = f.freshVarName2("head", "tail")
      MapCons(expr, lambda2[T1, F[T1], F[T2]](head, tail, App(f, Var[T1](head))))
    }

    def withFirst2[T1, T2](expr: Expr[F[T1]], f: Expr[T1 => T1 => F[T2]]): Expr[F[T2]] = {
      val (head1, tail1, head2, tail2) = f.freshVarName4("head1", "tail1", "head2", "tail2")
      MapCons(
        expr,
        lambda2[T1, F[T1], F[T2]](
          head1,
          tail1,
          MapCons(Var[F[T1]](tail1), lambda2[T1, F[T1], F[T2]](head2, tail2, app2(f, Var(head1), Var(head2))))
        )
      )
    }

    def withFirst3[T1, T2](expr: Expr[F[T1]], f: Expr[T1 => T1 => T1 => F[T2]]): Expr[F[T2]] = {
      val (head1, tail1, head2, tail2) = f.freshVarName4("head1", "tail1", "head2", "tail2")
      val (head3, tail3) = f.freshVarName2("head3", "tail3")
      MapCons(
        expr,
        lambda2[T1, F[T1], F[T2]](
          head1,
          tail1,
          MapCons(
            Var[F[T1]](tail1),
            lambda2[T1, F[T1], F[T2]](
              head2,
              tail2,
              MapCons(
                Var[F[T1]](tail2),
                lambda2[T1, F[T1], F[T2]](head3, tail3, app3(f, Var(head1), Var(head2), Var(head3)))
              )
            )
          )
        )
      )
    }

    def lambda2[A, B, C](var1: String, var2: String, expr: Expr[C]): Expr[A => B => C] =
      Lambda[A, B => C](var1, Lambda[B, C](var2, expr))

    def lambda3[A, B, C, D](var1: String, var2: String, var3: String, expr: Expr[D]): Expr[A => B => C => D] =
      Lambda[A, B => C => D](var1, lambda2[B, C, D](var2, var3, expr))

    private def app2[A, B, C](f: Expr[A => B => C], a: Expr[A], b: Expr[B]): Expr[C] =
      App(App(f, a), b)

    private def app3[A, B, C, D](f: Expr[A => B => C => D], a: Expr[A], b: Expr[B], c: Expr[C]): Expr[D] =
      App(App(App(f, a), b), c)
  }
}
