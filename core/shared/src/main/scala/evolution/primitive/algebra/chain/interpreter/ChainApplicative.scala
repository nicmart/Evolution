package evolution.primitive.algebra.chain.interpreter
import cats.Applicative
import evolution.primitive.algebra.Composed
import evolution.primitive.algebra.chain.Chain

class ChainApplicative[F[_], R[_], G[_]](alg: Chain[F, R])(implicit G: Applicative[G])
    extends Chain[F, Composed[G, R, ?]] {
  override def empty[A]: G[R[F[A]]] =
    G.pure(alg.empty)
  override def cons[A](head: G[R[A]], tail: G[R[F[A]]]): G[R[F[A]]] =
    G.map2(head, tail)(alg.cons)
  override def mapEmpty[A](eva: G[R[F[A]]], eva2: G[R[F[A]]]): G[R[F[A]]] =
    G.map2(eva, eva2)((rfa1, rfa2) => alg.mapEmpty(rfa1, rfa2))
  override def mapCons[A, B](eva: G[R[F[A]]])(f: G[R[A => F[A] => F[B]]]): G[R[F[B]]] =
    G.map2(eva, f)((rfa, rf) => alg.mapCons(rfa)(rf))
}
