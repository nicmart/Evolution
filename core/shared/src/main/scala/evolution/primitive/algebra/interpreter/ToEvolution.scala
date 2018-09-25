package evolution.primitive.algebra.interpreter

import cats.kernel.Semigroup
import cats.syntax.semigroup._
import evolution.algebra.EvolutionCoreAlgebra
import evolution.primitive.algebra._
import evolution.geometry.Point

// Generic TODO: Make sure we do as much as possible outside the closures
class ToEvolution[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F])
    extends DrawingAlgebra[Id, F, EvaluationResult, String] {
  override val drawing: CoreDrawingAlgebra[Id, F, EvaluationResult] =
    new CoreDrawingAlgebra[Id, F, EvaluationResult] {
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
  override val scalar: ScalarAlgebra[EvaluationResult] = new ScalarAlgebra[EvaluationResult] {
    override def double(d: Double): EvaluationResult[Double] = Value(_ => d)
    override def point(x: Double, y: Double): EvaluationResult[Point] = Value(_ => Point(x, y))
    override def add[T: Semigroup](a: EvaluationResult[T], b: EvaluationResult[T]): EvaluationResult[T] =
      Value(ctx => a.get(ctx) |+| b.get(ctx))
  }
  override val bind: BindingAlgebra[EvaluationResult, String] = BindingAlgebraEvaluator
}
