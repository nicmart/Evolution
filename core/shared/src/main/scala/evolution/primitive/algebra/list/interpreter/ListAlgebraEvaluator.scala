package evolution.primitive.algebra.list.interpreter
import cats.Id
import evolution.algebra.EvolutionCoreAlgebra
import evolution.primitive.algebra.binding.interpreter.{EvaluationResult, Value}
import evolution.primitive.algebra.list.ListAlgebra

class ListAlgebraEvaluator[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F]) extends ListAlgebra[Id, F, EvaluationResult] {
  override def empty[A]: EvaluationResult[F[A]] =
    Value(_ => evolutionAlg.empty)
  override def cons[A](head: EvaluationResult[A], tail: EvaluationResult[F[A]]): EvaluationResult[F[A]] =
    Value(ctx => evolutionAlg.cons(head.get(ctx), tail.get(ctx)))
  override def mapEmpty[A](eva: EvaluationResult[F[A]])(eva2: EvaluationResult[F[A]]): EvaluationResult[F[A]] =
    Value(ctx => evolutionAlg.mapEmpty(eva.get(ctx))(eva2.get(ctx)))
  override def mapCons[A, B](
    eva: EvaluationResult[F[A]]
  )(f: EvaluationResult[A => F[A] => F[B]]): EvaluationResult[F[B]] =
    Value(
      ctx =>
        evolutionAlg.mapCons(eva.get(ctx)) { (a: A, tail: F[A]) =>
          f.get(ctx)(a)(tail)
      }
    )
}