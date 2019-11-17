package evolution.compiler.impl.evaluation.model

import evolution.compiler.impl.evaluation.EvalCtx

private[evaluation] sealed trait Contextual[+T] { self =>
  def apply(ctx: EvalCtx): T
  final def map[S](f: T => S): Contextual[S] = Contextual.map(this, f)
  final def isPure: Boolean = this match {
    case Contextual.Pure(_) => true
    case _                  => false
  }
}

private[evaluation] object Contextual {
  case class Pure[T](t: T) extends Contextual[T] {
    override def apply(ctx: EvalCtx): T = t
  }

  sealed abstract class WithContext[T] extends Contextual[T]

  object WithContext {
    def instance[T](f: EvalCtx => T): Contextual[T] = new WithContext[T] {
      override def apply(ctx: EvalCtx): T = f(ctx)
    }
  }

  def lst[T](ts: List[Contextual[T]]): Contextual[List[T]] =
    if (ts.forall(_.isPure)) {
      Pure(ts.collect { case Pure(t) => t })
    } else
      new WithContext[List[T]] {
        def apply(ctx: EvalCtx): List[T] = ts.map(_.apply(ctx))
      }

  def map[A, B](a: Contextual[A], f: A => B): Contextual[B] = a match {
    case Pure(t)    => Pure(f(t))
    case contextual => new WithContext[B] { override def apply(ctx: EvalCtx): B = f(contextual(ctx)) }
  }

  def map2[A, B, C](a: Contextual[A], b: Contextual[B])(f: (A, B) => C): Contextual[C] =
    (a, b) match {
      case (Pure(ac), Pure(bc)) => Pure(f(ac, bc))
      case _                    => new WithContext[C] { override def apply(ctx: EvalCtx): C = f(a(ctx), b(ctx)) }
    }

  def map3[A, B, C, D](a: Contextual[A], b: Contextual[B], c: Contextual[C])(f: (A, B, C) => D): Contextual[D] =
    (a, b, c) match {
      case (Pure(ac), Pure(bc), Pure(cc)) => Pure(f(ac, bc, cc))
      case _                              => new WithContext[D] { override def apply(ctx: EvalCtx): D = f(a(ctx), b(ctx), c(ctx)) }
    }

  def map3Lazy[A, B, C, D](a: Contextual[A], b: Contextual[B], c: Contextual[C])(
      f: (=> A, => B, => C) => D
  ): Contextual[D] = new WithContext[D] { override def apply(ctx: EvalCtx): D = f(a(ctx), b(ctx), c(ctx)) }
}
