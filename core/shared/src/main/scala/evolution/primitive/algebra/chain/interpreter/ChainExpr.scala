package evolution.primitive.algebra.chain.interpreter
import cats.Id
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.evolution.Evolution
import evolution.primitive.algebra.evolution.Evolution.Expr

class ChainExpr[F[_]] extends Chain[F, Expr[F, ?]] {

  override def empty[A]: Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[A]] = alg.chain.empty
    }

  override def cons[A](head: Expr[F, A], tail: Expr[F, F[A]]): Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[A]] =
        alg.chain.cons(head.run(alg), tail.run(alg))
    }

  override def mapEmpty[A](eva: Expr[F, F[A]], eva2: Expr[F, F[A]]): Expr[F, F[A]] =
    new Expr[F, F[A]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[A]] =
        alg.chain.mapEmpty(eva.run(alg), eva2.run(alg))
    }

  override def mapCons[A, B](eva: Expr[F, F[A]])(f: Expr[F, A => F[A] => F[B]]): Expr[F, F[B]] =
    new Expr[F, F[B]] {
      override def run[R[_]](alg: Evolution[F, R]): R[F[B]] =
        alg.chain.mapCons(eva.run(alg))(f.run(alg))
    }
}
