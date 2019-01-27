package evolution.data
import cats.Group
import cats.kernel.{ Eq, Semigroup }
import evolution.data.MaterializationModuleImpl.E.{ Finite, Full }
import evolution.geometry.Point
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.chain.Chain
import evolution.primitive.algebra.constants.Constants
import evolution.primitive.algebra.derived.{ DefaultDerived, Derived }
import evolution.primitive.algebra.distribution.Distribution
import evolution.primitive.algebra.evolution.Evolution
import evolution.typeclass.VectorSpace

trait MaterializationModule {
  type F[T]
  type R[T]

  def evolution: Evolution[F, R]

  def materialize[T](seed: Long, ft: R[F[T]]): Iterator[T]
}

private[data] object MaterializationModuleImpl extends MaterializationModule {
  override def materialize[T](seed: Long, ft: R[F[T]]): Iterator[T] = ???

  type F[T] = initial.F[T]
  type R[T] = initial.R[T]

  override def evolution: Evolution[F, R] = initial.evolution

  import initial._

  private def usedVars[T](rt: R[T]): Set[Int] =
    rt match {
      case Evo(evo)            => usedVars(evo)
      case Dbl(d)              => Set.empty
      case Pnt(x, y)           => usedVars(x) ++ usedVars(y)
      case Add(a, b)           => usedVars(a) ++ usedVars(b)
      case Multiply(k, t)      => usedVars(k) ++ usedVars(t)
      case Sin(d)              => usedVars(d)
      case Cos(d)              => usedVars(d)
      case Var0(name)          => Set(0)
      case Shift(expr)         => usedVars(expr).map(_ + 1)
      case Let(_, value, expr) => usedVars(value) ++ usedVars(expr).map(_ - 1).filter(_ >= 0)
      case Lambda(_, expr)     => usedVars(expr).map(_ - 1).filter(_ >= 0)
      case App(f, a)           => usedVars(f) ++ usedVars(a)
      case Fix(expr)           => usedVars(expr).map(_ - 1).filter(_ >= 0)
      case Equals(a, b) => usedVars(a) ++ usedVars(b)
    }

  private def usedVars[T](evo: F[T]): Set[Int] = evo match {
    case Empty()             => Set.empty
    case Cons(head, tail)    => usedVars(head) ++ usedVars(tail)
    case MapEmpty(eva, eva2) => usedVars(eva) ++ usedVars(eva2)
    case MapCons(eva, f)     => usedVars(eva) ++ usedVars(f)
  }

  private def assign[A, B](rt: R[A], variable: Int, value: R[B]): R[A] =
    rt match {
      case Evo(get)                => Evo(assign(get, variable, value))
      case Dbl(d)                  => Dbl(d)
      case Pnt(x, y)               => Pnt(assign(x, variable, value), assign(y, variable, value))
      case add @ Add(a, b)         => add.map2(assign(_, variable, value), assign(_, variable, value))
      case mult @ Multiply(k, t)   => mult.map2(assign(_, variable, value), assign(_, variable, value))
      case Sin(d)                  => Sin(assign(d, variable, value))
      case Cos(d)                  => Cos(assign(d, variable, value))
      case Var0(name)              => if (variable == 0) value.asInstanceOf[R[A]] else Var0(name)
      case Shift(expr)             => Shift(assign(expr, variable - 1, value))
      case Let(v, letVal, letExpr) => Let(v, assign(letVal, variable, value), assign(letExpr, variable, value))
      case Lambda(v, expr)         => Lambda(v, assign(expr, variable, value))
      case App(f, a)               => App(assign(f, variable, value), assign(a, variable, value))
      case Fix(expr)               => Fix(assign(expr, variable, value))
    }

  private def assign[A, B](fa: F[A], variable: Int, value: R[B]): F[A] =
    fa match {
      case Empty()             => Empty()
      case Cons(head, tail)    => Cons(assign(head, variable, value), assign(tail, variable, value))
      case MapEmpty(eva, eva2) => MapEmpty(assign(eva, variable, value), assign(eva2, variable, value))
      case MapCons(eva, f)     => MapCons(assign(eva, variable, value), assign(f, variable, value))
      case Uniform(from, to)   => Uniform(assign(from, variable, value), assign(to, variable, value))
    }

  import EvaluationContextModule._, EvaluationContextModule._

  private def eval[T](ctx: Ctx, rt: R[T]): T = rt match {
    case Evo(evo)              => evo
    case Dbl(d)                => d
    case Pnt(x, y)             => Point(eval(ctx, x), eval(ctx, y))
    case add @ Add(a, b)       => add.semigroup.combine(eval(ctx, a), eval(ctx, b))
    case mult @ Multiply(k, t) => mult.vectorSpace.mult(eval(ctx, k), eval(ctx, t))
    case Sin(d)                => Math.sin(eval(ctx, d))
    case Cos(d)                => Math.cos(eval(ctx, d))
    case Var0(name)            => ctx(0)
    case Shift(expr)           => eval(ctx.pop, expr)
    case Let(_, value, expr)   => eval(ctx.pushStrict(eval(ctx, value), ""), expr)
    case Lambda(_, expr)       => (a => eval(ctx.pushStrict(a, ""), expr))
    case App(f, a)             => eval(ctx, f)(eval(ctx, a))
    case Fix(Lambda(_, expr))  => eval(ctx.pushLazy(() => eval(ctx, expr), ""), expr)
    case Fix(expr)             => ??? // This will diverge
  }

  sealed trait E[T]
  object E {
    final case class Full[T](run: Long => (Long, Option[(T, E[T])])) extends E[T]
    final case class Finite[T](ts: List[T]) extends E[T]
  }

  private def evalEvo[T](ctx: Ctx, rft: R[F[T]]): E[T] = eval(ctx, eval(ctx, rft))

  private def eval[T](ctx: Ctx, ft: F[T]): E[T] = ft match {

    case Empty() => E.Finite(Nil)

    case Cons(head, tail) =>
      evalEvo(ctx, tail) match {
        case Finite(ts) => Finite(eval(ctx, head) :: ts)
        case full @ Full(_) =>
          Full { seed =>
            (seed, Some((eval(ctx, head), full)))
          }
      }

    case MapEmpty(eva, eva2) =>
      evalEvo(ctx, eva) match {
        case Finite(Nil) => evalEvo(ctx, eva2)
        case Full(thisRun) =>
          Full { seed =>
            val (seed2, maybeNext) = thisRun(seed)
            maybeNext match {
              case None => run(seed2, evalEvo(ctx, eva2))
              case next => (seed2, next)
            }
          }
      }

    case MapCons(eva, f)   => ???
    case Uniform(from, to) => ???
  }

  private def run[T](seed: Long, evo: E[T]): (Long, Option[(T, E[T])]) = evo match {
    case Full(doRun) => doRun(seed)
    case Finite(ts) =>
      ts match {
        case head :: tail => (seed, Some((head, Finite(tail))))
        case Nil          => (seed, None)
      }
  }

  private def isConstantLambda[A, B](f: R[A => B]): Option[R[B]] =
    f match {
      case Lambda(variable, expr) => Some(expr).filter(usedVars(_).contains(0))
      case _                      => None
    }
}
