package evolution.primitive.algebra.interpreter

import cats.kernel.Group
import cats.implicits._
import evolution.algebra
import evolution.algebra.{Evolution, EvolutionCoreAlgebra}
import evolution.primitive.algebra._
import evolution.geometry.Point
import evolution.algebra.syntax.all._

//class ToEvolution[F[+ _]](evolutionAlg: EvolutionCoreAlgebra[F]) extends DrawingAlgebra[CtxScalar, CtxF[F, ?]] {
//  override val drawing: CoreDrawingAlgebra[CtxScalar, CtxF[F, ?]] =
//    new CoreDrawingAlgebra[CtxScalar, CtxF[F, ?]] {
//      override def empty[A]: CtxF[F, A] =
//        _ => evolutionAlg.empty
//      override def cons[A](head: CtxScalar[A], tail: CtxF[F, A]): CtxF[F, A] =
//        ctx => evolutionAlg.cons(head(ctx), tail(ctx))
//      override def mapEmpty[A](eva: CtxF[F, A])(eva2: CtxF[F, A]): CtxF[F, A] =
//        ctx => evolutionAlg.mapEmpty(eva(ctx))(eva2(ctx))
//      override def mapCons[A, B](eva: CtxF[F, A])(f: CtxF[F, B]): CtxF[F, B] =
//        ctx =>
//          evolutionAlg.mapCons(eva(ctx)) { (a: A, tail: F[A]) =>
//            f((() => a) :: (() => tail) :: ctx)
//        }
//    }
//  override val scalar: ScalarAlgebra[CtxScalar] = new ScalarAlgebra[CtxScalar] {
//    override def double(d: Double): CtxScalar[Double] = _ => d
//    override def point(p: Point): CtxScalar[Point] = _ => p
//  }
//  override val bindS: BindingAlgebra[CtxScalar] = new BindingAlgebra[CtxScalar] {
//    override def var0[A]: CtxScalar[A] = {
//      case h :: tail => h().asInstanceOf[A]
//    }
//    override def shift[A](expr: CtxScalar[A]): CtxScalar[A] = {
//      case h :: tail => expr(tail)
//    }
//    override def let[A, B](name: String, value: CtxScalar[A])(expr: CtxScalar[B]): CtxScalar[B] =
//      ctx => expr((() => value(ctx)) :: ctx)
//
//    override def fix[A](expr: CtxScalar[A]): CtxScalar[A] =
//      ctx => expr((() => expr(ctx)) :: ctx)
//
//    override def lambda[A, B](name: String, expr: CtxScalar[B]): CtxScalar[B] = expr
//  }
//  override val bindF: BindingAlgebra[CtxF[F, ?]] = new BindingAlgebra[CtxF[F, ?]] {
//    override def var0[A]: CtxF[F, A] = {
//      case h :: tail => h.asInstanceOf[F[A]]
//    }
//    override def shift[A](expr: CtxF[F, A]): CtxF[F, A] = {
//      case h :: tail => expr(tail)
//    }
//    override def let[A, B](name: String, value: CtxF[F, A])(expr: CtxF[F, B]): CtxF[F, B] =
//      ctx => expr((() => value(ctx)) :: ctx)
//
//    override def fix[A](expr: CtxF[F, A]): CtxF[F, A] =
//      ctx => expr((() => expr(ctx)) :: ctx)
//
//    override def lambda[A, B](name: String, expr: CtxF[F, B]): CtxF[F, B] = expr
//  }
//}
