package evolution.primitive.algebra.chain.interpreter
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class ChainExpr[S[_], F[_]] extends Chain[S, F, Expr[S, F, ?]] {

  override def empty[A]: Expr[S, F, F[A]] =
    new Expr[S, F, F[A]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[F[A]] = alg.list.empty
    }

  override def cons[A](head: Expr[S, F, S[A]], tail: Expr[S, F, F[A]]): Expr[S, F, F[A]] =
    new Expr[S, F, F[A]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[F[A]] =
        alg.list.cons(head.run(alg), tail.run(alg))
    }

  override def mapEmpty[A](eva: Expr[S, F, F[A]], eva2: Expr[S, F, F[A]]): Expr[S, F, F[A]] =
    new Expr[S, F, F[A]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[F[A]] =
        alg.list.mapEmpty(eva.run(alg), eva2.run(alg))
    }

  override def mapCons[A, B](eva: Expr[S, F, F[A]])(f: Expr[S, F, S[A] => F[A] => F[B]]): Expr[S, F, F[B]] =
    new Expr[S, F, F[B]] {
      override def run[R[_]](alg: Evolution[S, F, R, Double, String, String]): R[F[B]] =
        alg.list.mapCons(eva.run(alg))(f.run(alg))
    }
}
