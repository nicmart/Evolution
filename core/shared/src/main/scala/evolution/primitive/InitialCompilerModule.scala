package evolution.primitive

import evolution.algebra.representation.RNGRepr
import evolution.data.{ Ctx, Initial }
import evolution.data.EvaluationContext._
import evolution.geometry.Point

object InitialCompilerModule {
  private val initial: Initial[RNGRepr] = new Initial[RNGRepr] {}
  import initial._
  type Out[T] = Ctx => T

  def compile[T](expr: R[T]): Out[T] = expr match {
    case Dbl(d)                => Out.pure(d)
    case Floor(d)              => compile(d).map(_.toInt)
    case Integer(n)            => Out.pure(n)
    case Pnt(x, y)             => compile2(x, y)(Point.apply)
    case X(p)                  => compile1(p)(_.x)
    case Y(p)                  => compile1(p)(_.y)
    case add @ Add(a, b)       => compile2(a, b)(add.semigroup.combine)
    case Div(a, b)             => compile2(a, b)(_ / _)
    case Exp(a, b)             => compile2(a, b)(Math.pow)
    case inv @ Inverse(t)      => compile1(t)(inv.group.inverse)
    case mult @ Multiply(k, t) => compile2(k, t)(mult.vectorSpace.mult)
    case Sin(d)                => compile1(d)(Math.sin)
    case Cos(d)                => compile1(d)(Math.cos)
    case eq @ Equals(a, b)     => compile2(a, b)(eq.eq.eqv)
    case IfThen(condition, a, b) =>
      compile3(condition, a, b) { (compiledCondition, compiledA, compiledB) =>
        if (compiledCondition) compiledA else compiledB
      }

    case Var0(name) =>
      ctx =>
        get(ctx, 0)

    case Shift(e) =>
      ctx =>
        compile(e)(pop(ctx))

    case Let(_, value, e) =>
      ctx =>
        compile(e)(pushStrict(compile(value)(ctx), ctx, ""))

    case Lambda(_, e) =>
      ctx => a =>
        compile(e)(pushStrict(a, ctx, ""))

    case App(f, a) => compile2(f, a)(_(_))

    case Fix(Lambda(_, lambdaBody)) =>
      ctx =>
        {
          lazy val a: T = compile(lambdaBody)(pushLazy(() => a, ctx, ""))
          a
        }

    case Fix(_) => ???

    case Empty() =>
      Out.pure(RNGRepr { rng =>
        (rng, None)
      })

    case Cons(head, tail) =>
      Out.map2(compile(head), compile(tail)) { (compiledHead, compiledTail) =>
        RNGRepr { rng =>
          (rng, Some((compiledHead, compiledTail)))
        }
      }

    case MapEmpty(ev1, ev2) =>
      Out.map2(compile(ev1), compile(ev2)) { (compiled1, compiled2) =>
        RNGRepr { rng =>
          val (rng2, next) = compiled1.run(rng)
          next match {
            case None => compiled2.run(rng2)
            case _    => (rng2, next)
          }
        }
      }

    case MapCons(eva, f) =>
      Out.map2(compile(eva), compile(f)) { (compiledEva, compiledF) =>
        RNGRepr { rng =>
          val (rng2, next) = compiledEva.run(rng)
          next match {
            case None            => (rng2, None)
            case Some((a, eva2)) => compiledF(a)(eva2).run(rng2)
          }
        }
      }

    case Uniform(from, to) =>
      Out.map2(compile(from), compile(to)) { (compiledFrom, compiledTo) =>
        lazy val self: RNGRepr[Double] = RNGRepr { rng =>
          val (n, rng2) = rng.nextInt
          val d = (n.toDouble - Int.MinValue) / (Int.MaxValue.toDouble - Int.MinValue.toDouble)
          val scaled = compiledFrom + d * (compiledTo - compiledFrom)
          (rng2, Some((scaled, self)))
        }
        self
      }

    case UniformDiscrete(from, to, step) =>
      Out.map3(compile(from), compile(to), compile(step)) { (f, t, s) =>
        uniformChoiceRepr((f to t by s).toList)
      }

    case UniformChoice(ts) =>
      ctx =>
        uniformChoiceRepr(ts.map(t => compile(t)(ctx)))

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

  private def compile1[A, B](a: R[A])(f: A => B): Out[B] =
    compile(a).map(f)

  private def compile2[A, B, C](a: R[A], b: R[B])(f: (A, B) => C): Out[C] =
    Out.map2(compile(a), compile(b))(f)

  private def compile3[A, B, C, D](a: R[A], b: R[B], c: R[C])(f: (A, B, C) => D): Out[D] =
    Out.map3(compile(a), compile(b), compile(c))(f)

  private object Out {
    def pure[T](t: T): Out[T] = ctx => t
    def map2[A, B, C](a: Out[A], b: Out[B])(f: (A, B) => C): Out[C] =
      ctx => f(a(ctx), b(ctx))
    def map3[A, B, C, D](a: Out[A], b: Out[B], c: Out[C])(f: (A, B, C) => D): Out[D] =
      ctx => f(a(ctx), b(ctx), c(ctx))
  }
}
