package evolution.compiler.phases.materializing.model

import evolution.data.Ctx

sealed trait Contextual[+T] { self =>
  def apply(ctx: Ctx): T
  final def map[S](f: T => S): Contextual[S] = Contextual.map(this, f)
}

object Contextual {
  case class Pure[T](t: T) extends Contextual[T] {
    override def apply(ctx: Ctx): T = t
  }

  sealed abstract class WithContext[T] extends Contextual[T]

  object WithContext {
    def instance[T](f: Ctx => T): Contextual[T] = new WithContext[T] {
      override def apply(ctx: Ctx): T = f(ctx)
    }
  }

  def map[A, B](a: Contextual[A], f: A => B): Contextual[B] = a match {
    case Pure(t)    => Pure(f(t))
    case contextual => new WithContext[B] { override def apply(ctx: Ctx): B = f(contextual(ctx)) }
  }

  def map2[A, B, C](a: Contextual[A], b: Contextual[B])(f: (A, B) => C): Contextual[C] =
    (a, b) match {
      case (Pure(ac), Pure(bc)) => Pure(f(ac, bc))
      case _                    => new WithContext[C] { override def apply(ctx: Ctx): C = f(a(ctx), b(ctx)) }
    }

  def map3[A, B, C, D](a: Contextual[A], b: Contextual[B], c: Contextual[C])(f: (A, B, C) => D): Contextual[D] =
    (a, b, c) match {
      case (Pure(ac), Pure(bc), Pure(cc)) => Pure(f(ac, bc, cc))
      case _                              => new WithContext[D] { override def apply(ctx: Ctx): D = f(a(ctx), b(ctx), c(ctx)) }
    }

  def map3Lazy[A, B, C, D](a: Contextual[A], b: Contextual[B], c: Contextual[C])(
    f: (=> A, => B, => C) => D
  ): Contextual[D] = new WithContext[D] { override def apply(ctx: Ctx): D = f(a(ctx), b(ctx), c(ctx)) }
}
