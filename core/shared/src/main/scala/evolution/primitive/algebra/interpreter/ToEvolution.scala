package evolution.primitive.algebra.interpreter

import evolution.algebra.EvolutionCoreAlgebra
import evolution.primitive.algebra._
import evolution.geometry.Point

class ToEvolution[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F]) extends DrawingAlgebra[Id, F, Ctx] {
  type CtxF[T] = Ctx[F[T]]
  override val drawing: CoreDrawingAlgebra[Ctx, CtxF] =
    new CoreDrawingAlgebra[Ctx, CtxF] {
      override def empty[A]: Ctx[F[A]] =
        _ => evolutionAlg.empty
      override def cons[A](head: Ctx[A], tail: Ctx[F[A]]): Ctx[F[A]] =
        ctx => evolutionAlg.cons(head(ctx), tail(ctx))
      override def mapEmpty[A](eva: Ctx[F[A]])(eva2: Ctx[F[A]]): Ctx[F[A]] =
        ctx => evolutionAlg.mapEmpty(eva(ctx))(eva2(ctx))
      override def mapCons[A, B](eva: Ctx[F[A]])(f: Ctx[F[B]]): Ctx[F[B]] =
        ctx =>
          evolutionAlg.mapCons(eva(ctx)) { (a: A, tail: F[A]) =>
            f((() => a) :: (() => tail) :: ctx)
        }
    }
  override val scalar: ScalarAlgebra[Ctx] = new ScalarAlgebra[Ctx] {
    override def double(d: Double): Ctx[Double] = _ => d
    override def point(p: Point): Ctx[Point] = _ => p
  }
  override val bind: BindingAlgebra[Ctx] = new BindingAlgebra[Ctx] {
    override def var0[A]: Ctx[A] = {
      case h :: tail => h().asInstanceOf[A]
    }
    override def shift[A](expr: Ctx[A]): Ctx[A] = {
      case h :: tail => expr(tail)
    }
    override def let[A, B](name: String, value: Ctx[A])(expr: Ctx[B]): Ctx[B] =
      ctx => expr((() => value(ctx)) :: ctx)

    override def fix[A](expr: Ctx[A]): Ctx[A] =
      ctx => expr((() => fix(expr)(ctx)) :: ctx)

    override def lambda[A, B](name: String, expr: Ctx[B]): Ctx[B] = expr
  }
}
