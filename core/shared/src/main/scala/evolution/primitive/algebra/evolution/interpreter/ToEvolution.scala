package evolution.primitive.algebra.evolution.interpreter

import cats.Id
import cats.kernel.Semigroup
import cats.syntax.semigroup._
import evolution.algebra.EvolutionCoreAlgebra
import evolution.geometry.Point
import evolution.primitive.algebra.binding.BindingAlgebra
import evolution.primitive.algebra.binding.interpreter.{BindingAlgebraEvaluator, EvaluationResult, Value}
import evolution.primitive.algebra.constants.ConstantsAlgebra
import evolution.primitive.algebra.evolution.EvolutionAlgebra
import evolution.primitive.algebra.list.ListAlgebra

// Generic TODO: Make sure we do as much as possible outside the closures
class ToEvolution[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F])
    extends EvolutionAlgebra[Id, F, EvaluationResult, String] {
  override val drawing: ListAlgebra[Id, F, EvaluationResult] =
    new ListAlgebra[Id, F, EvaluationResult] {
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
  override val scalar: ConstantsAlgebra[EvaluationResult] = new ConstantsAlgebra[EvaluationResult] {
    override def double(d: Double): EvaluationResult[Double] = Value(_ => d)
    override def point(x: Double, y: Double): EvaluationResult[Point] = Value(_ => Point(x, y))
    override def add[T: Semigroup](a: EvaluationResult[T], b: EvaluationResult[T]): EvaluationResult[T] =
      Value(ctx => a.get(ctx) |+| b.get(ctx))
  }
  override val bind: BindingAlgebra[EvaluationResult, String] = BindingAlgebraEvaluator
}
