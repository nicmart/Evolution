package evolution.primitive.algebra.chain.interpreter
import evolution.data.AnnotationModule._
import evolution.primitive.algebra.chain.Chain

// TODO through applicative?
object ChainEvaluator extends Chain[F, R] {

  override def empty[A]: R[F[A]] =
    Annotation(Set.empty, builder.chain.empty)

  override def cons[A](evalHead: R[A], evalTail: R[F[A]]): R[F[A]] =
    Annotation(evalHead.vars ++ evalTail.vars, builder.chain.cons(evalHead.expr, evalTail.expr))

  override def mapEmpty[A](eva: R[F[A]], eva2: R[F[A]]): R[F[A]] =
    Annotation(eva.vars ++ eva2.vars, builder.chain.mapEmpty(eva.expr, eva2.expr))

  override def mapCons[A, B](evalFA: R[F[A]])(evalF: R[A => F[A] => F[B]]): R[F[B]] =
    Annotation(evalFA.vars ++ evalF.vars, builder.chain.mapCons(evalFA.expr)(evalF.expr))
}
