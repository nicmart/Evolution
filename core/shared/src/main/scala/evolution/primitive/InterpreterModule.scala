package evolution.primitive

import evolution.algebra.representation.RNGRepr
import evolution.data.{ Ctx, ExpressionModule, WithExpression }
import evolution.data.EvaluationContext._
import evolution.geometry.Point

// TODO this is an implementation
trait InterpreterModule { self: WithExpression[RNGRepr] =>
  type Out[T] = InterpreterModule.Out[T]
  import expressionModule._
  import InterpreterModule._

  object Interpreter {
    def interpret[T](expr: Expr[T]): Out[T] = expr match {
      case Dbl(d)          => Out.pure(d)
      case Floor(d)        => interpret(d).map(_.toInt)
      case Integer(n)      => Out.pure(n)
      case Pnt(x, y)       => interpret2(x, y)(Point.apply)
      case X(p)            => interpret1(p)(_.x)
      case Y(p)            => interpret1(p)(_.y)
      case add @ Add(a, b) => interpret2(a, b)(add.semigroup.combine)
      case Div(a, b)       => interpret2(a, b)(_ / _)
      case Exp(a, b)       => interpret2(a, b)(Math.pow)
      case Mod(a, b) =>
        interpret2(a, b) { (ca, cb) =>
          if (ca >= 0) ca % cb else (ca % cb) + cb
        }
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
        new Contextual[T] {
          override def apply(ctx: Ctx): T = get(ctx, 0).asInstanceOf[T]
        }

      case Shift(e) =>
        val interpretedE = interpret(e)
        new Contextual[T] {
          override def apply(ctx: Ctx): T = interpretedE(pop(ctx))
        }

      case Let(name, value, e) =>
        interpret(App(Lambda(name, e), value))

      case Lambda(_, body) =>
        val interpretedBody = interpret(body)
        new Contextual[T] {
          override def apply(ctx: Ctx): T = a => interpretedBody(pushStrict(a, ctx, ""))
        }

      case App(f, a) => interpret2(f, a)(_(_))

      // Detect constant evolutions
      case Fix(Lambda(_, Cons(t, Var0(_)))) =>
        ConstantEvolution(interpret(Expr.unshift(t)))

      case Fix(Lambda(_, lambdaBody)) =>
        val interpretedBody = interpret(lambdaBody)
        new Contextual[T] {
          override def apply(ctx: Ctx): T = {
            lazy val a: T = interpretedBody(pushLazy(() => a, ctx, ""))
            a
          }
        }

      case Fix(_) => ???

      case Empty() =>
        Out.pure(RNGRepr { rng =>
          (rng, None)
        })

      // TODO we need a well-defined strategy for lazyness. In this case, we deley the materialization of cons, to allow
      // recursive definitions
      case Cons(head, tail) =>
        val interpretedHead = interpret(head)
        val interpretedTail = interpret(tail)
        new Contextual[T] {
          override def apply(ctx: Ctx): T = {
            val h = interpretedHead(ctx)
            RNGRepr { rng =>
              (rng, Some((h, interpretedTail(ctx))))
            }
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
        new Contextual[T] {
          override def apply(ctx: Ctx): T = uniformChoiceRepr(ts.map(t => interpret(t)(ctx)))
        }
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

    private def interpret1[A, B](a: Expr[A])(f: A => B): Out[B] =
      interpret(a).map(f)

    private def interpret2[A, B, C](a: Expr[A], b: Expr[B])(f: (A, B) => C): Out[C] =
      Out.map2(interpret(a), interpret(b))(f)

    private def interpret3[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C])(f: (A, B, C) => D): Out[D] =
      Out.map3(interpret(a), interpret(b), interpret(c))(f)
  }
}

object InterpreterModule {

  sealed trait Out[T] { self =>
    def apply(ctx: Ctx): T
    final def map[S](f: T => S): Out[S] = Out.map(this, f)
  }

  case class Constant[T](t: T) extends Out[T] {
    override def apply(ctx: Ctx): T = t
  }

  case class ConstantEvolution[T](t: Out[T]) extends Out[RNGRepr[T]] {
    override def apply(ctx: Ctx): RNGRepr[T] = {
      val tc = t(ctx)
      lazy val rngRepr: RNGRepr[T] = RNGRepr[T] { rng =>
        (rng, Some((tc, rngRepr)))
      }
      rngRepr
    }
  }

  sealed abstract class Contextual[T] extends Out[T]

  object Out {
    def pure[T](t: T): Out[T] = Constant(t)
    def map[A, B](a: Out[A], f: A => B): Out[B] = a match {
      case Constant(t)               => Constant(f(t))
      case contextual: Contextual[_] => new Contextual[B] { override def apply(ctx: Ctx): B = f(contextual(ctx)) }
    }

    def map2[A, B, C](a: Out[A], b: Out[B])(f: (A, B) => C): Out[C] =
      (a, b) match {
        case (Constant(ac), Constant(bc)) => Constant(f(ac, bc))
        case _                            => new Contextual[C] { override def apply(ctx: Ctx): C = f(a(ctx), b(ctx)) }
      }

    def map3[A, B, C, D](a: Out[A], b: Out[B], c: Out[C])(f: (A, B, C) => D): Out[D] =
      (a, b, c) match {
        case (Constant(ac), Constant(bc), Constant(cc)) => Constant(f(ac, bc, cc))
        case _                                          => new Contextual[D] { override def apply(ctx: Ctx): D = f(a(ctx), b(ctx), c(ctx)) }
      }
  }
}
