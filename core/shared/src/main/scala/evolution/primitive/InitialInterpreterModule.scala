package evolution.primitive

import evolution.algebra.representation.RNGRepr
import evolution.data.{ Ctx, Initial }
import evolution.data.EvaluationContext._
import evolution.geometry.Point

object InitialInterpreterModule {
  private val initial: Initial[RNGRepr] = new Initial[RNGRepr] {}
  import initial._
  type Out[T] = Ctx => T

  def interpret[T](expr: R[T]): Out[T] = expr match {
    case Dbl(d)                => Out.pure(d)
    case Floor(d)              => interpret(d).map(_.toInt)
    case Integer(n)            => Out.pure(n)
    case Pnt(x, y)             => interpret2(x, y)(Point.apply)
    case X(p)                  => interpret1(p)(_.x)
    case Y(p)                  => interpret1(p)(_.y)
    case add @ Add(a, b)       => interpret2(a, b)(add.semigroup.combine)
    case Div(a, b)             => interpret2(a, b)(_ / _)
    case Exp(a, b)             => interpret2(a, b)(Math.pow)
    case inv @ Inverse(t)      => interpret1(t)(inv.group.inverse)
    case mult @ Multiply(k, t) => interpret2(k, t)(mult.vectorSpace.mult)
    case Sin(d)                => interpret1(d)(Math.sin)
    case Cos(d)                => interpret1(d)(Math.cos)
    case eq @ Equals(a, b)     => interpret2(a, b)(eq.eq.eqv)
    case IfThen(condition, a, b) =>
      interpret3(condition, a, b) { (compiledCondition, compiledA, compiledB) =>
        if (compiledCondition) compiledA else compiledB
      }

    case Var0(name) =>
      ctx =>
        get(ctx, 0)

    case Shift(e) =>
      ctx =>
        interpret(e)(pop(ctx))

    case Let(_, value, e) =>
      ctx =>
        interpret(e)(pushStrict(interpret(value)(ctx), ctx, ""))

    case Lambda(_, e) =>
      ctx => a =>
        interpret(e)(pushStrict(a, ctx, ""))

    case App(f, a) => interpret2(f, a)(_(_))

    case Fix(Lambda(_, lambdaBody)) =>
      ctx =>
        {
          lazy val a: T = interpret(lambdaBody)(pushLazy(() => a, ctx, ""))
          a
        }

    case Fix(_) => ???

    case Empty() =>
      Out.pure(RNGRepr { rng =>
        (rng, None)
      })

    case Cons(head, tail) =>
      Out.map2(interpret(head), interpret(tail)) { (compiledHead, compiledTail) =>
        RNGRepr { rng =>
          (rng, Some((compiledHead, compiledTail)))
        }
      }

    case MapEmpty(ev1, ev2) =>
      Out.map2(interpret(ev1), interpret(ev2)) { (compiled1, compiled2) =>
        RNGRepr { rng =>
          val (rng2, next) = compiled1.run(rng)
          next match {
            case None => compiled2.run(rng2)
            case _    => (rng2, next)
          }
        }
      }

    case MapCons(eva, f) =>
      Out.map2(interpret(eva), interpret(f)) { (compiledEva, compiledF) =>
        RNGRepr { rng =>
          val (rng2, next) = compiledEva.run(rng)
          next match {
            case None            => (rng2, None)
            case Some((a, eva2)) => compiledF(a)(eva2).run(rng2)
          }
        }
      }

    case Uniform(from, to) =>
      Out.map2(interpret(from), interpret(to)) { (compiledFrom, compiledTo) =>
        lazy val self: RNGRepr[Double] = RNGRepr { rng =>
          val (n, rng2) = rng.nextInt
          val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
          val scaled = compiledFrom + d * (compiledTo - compiledFrom)
          (rng2, Some((scaled, self)))
        }
        self
      }

    case UniformDiscrete(from, to, step) =>
      Out.map3(interpret(from), interpret(to), interpret(step)) { (f, t, s) =>
        uniformChoiceRepr((f to t by s).toList)
      }

    case UniformChoice(ts) =>
      ctx =>
        uniformChoiceRepr(ts.map(t => interpret(t)(ctx)))

  }

  private def uniformChoiceRepr[T](ts: List[T]): RNGRepr[T] =
    ts match {
      case Nil =>
        RNGRepr[T] { rng =>
          (rng, None)
        }
      case _ =>
        lazy val self: RNGRepr[T] = RNGRepr { rng =>
          val (n, rng2) = rng.nextInt
          val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
          val index = (d * ts.size).toInt
          (rng2, Some((ts(index), self)))
        }
        self
    }

  private implicit class OutOps[A](val out: Out[A]) extends AnyVal {
    def map[B](f: A => B): Out[B] = ctx => f(out(ctx))

  }

  private def interpret1[A, B](a: R[A])(f: A => B): Out[B] =
    interpret(a).map(f)

  private def interpret2[A, B, C](a: R[A], b: R[B])(f: (A, B) => C): Out[C] =
    Out.map2(interpret(a), interpret(b))(f)

  private def interpret3[A, B, C, D](a: R[A], b: R[B], c: R[C])(f: (A, B, C) => D): Out[D] =
    Out.map3(interpret(a), interpret(b), interpret(c))(f)

  private object Out {
    def pure[T](t: T): Out[T] = ctx => t
    def map2[A, B, C](a: Out[A], b: Out[B])(f: (A, B) => C): Out[C] =
      ctx => f(a(ctx), b(ctx))
    def map3[A, B, C, D](a: Out[A], b: Out[B], c: Out[C])(f: (A, B, C) => D): Out[D] =
      ctx => f(a(ctx), b(ctx), c(ctx))
  }
}
