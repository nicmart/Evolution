package evolution.compiler.impl.evaluation

import evolution.materialization.Evolution
import evolution.data.EvaluationContext._
import evolution.compiler.expression.Expr
import evolution.geometry.Point
import evolution.compiler.phases.materializing.model.Contextual
import evolution.compiler.phases.materializing.model.Contextual.WithContext
import evolution.data.emptyCtx
import Expr._
import evolution.compiler.phases.materializing.Materializer

// TODO this is an implementation
object EvalMaterializer extends Materializer {

  def materialize[T](expr: Expr[Evolution[T]]): Long => Iterator[T] =
    seed => Evolution.runWithSeed(seed, materializeExpr(expr).apply(emptyCtx).asInstanceOf[Evolution[T]])

  def materializeExpr[T](expr: Expr[T]): Contextual[T] = {
    expr match {
      case Dbl(d)     => Contextual.Pure(d)
      case Floor(d)   => materializeExpr(d).map(_.toInt)
      case ToDbl(n)   => materializeExpr(n).map(_.toDouble)
      case Integer(n) => Contextual.Pure(n)
      case Pnt(x, y)  => interpret2(x, y)(Point.apply)
      case LiftedPnt(x, y) =>
        interpret2[Evolution[Double], Evolution[Double], Evolution[Point]](x, y)(
          Evolution.zipWithUncurried(Point.apply)
        )
      case Polar(x, y)       => interpret2(x, y)(Point.polar)
      case LiftedPolar(x, y) => interpret2(x, y)(Evolution.zipWithUncurried(Point.polar))
      case X(p)              => interpret1(p)(_.x)
      case Y(p)              => interpret1(p)(_.y)
      case Norm(p)           => interpret1(p)(_.norm)
      case Versor(p)         => interpret1(p)(_.versor)
      case Add(a, b, add)    => interpret2(a, b)(MaterializeAddition(add))
      case Div(a, b)         => interpret2(a, b)(_ / _)
      case Exp(a, b)         => interpret2(a, b)(Math.pow)
      case Abs(a)            => materializeExpr(a).map(Math.abs)
      case Sign(a)           => materializeExpr(a).map(Math.signum)
      case Mod(a, b) =>
        interpret2(a, b) { (ca, cb) =>
          if (ca >= 0) ca % cb else (ca % cb) + cb
        }
      case e @ Inverse(_, _)     => interpret1(e.t)(e.inv.inverse)
      case e @ Minus(_, _, _, _) => interpret2(e.a, e.b)((a, b) => MaterializeAddition(e.sg)(a, e.inv.inverse(b)))
      case e @ Multiply(_, _, _) => interpret2(e.a, e.b)(e.mult.combine)
      case Sin(d)                => interpret1(d)(Math.sin)
      case Cos(d)                => interpret1(d)(Math.cos)
      case Lst(ts)               => Contextual.lst(ts.map(materializeExpr))
      case SmoothStep(f, t, p) =>
        interpret3(f, t, p) { (from, to, position) =>
          val t = (position - from) / (to - from)
          if (t <= 0) 0.0
          else if (t >= 1) 1.0
          else t * t * (3.0 - 2.0 * t)
        }
      case Equals(a, b, eq) => interpret2(a, b)(eq.eqv)
      case Neq(a, b, eq)    => interpret2(a, b)(eq.neqv)
      case IfThen(condition, a, b) =>
        interpret3Lazy(condition, a, b) { (compiledCondition, compiledA, compiledB) =>
          if (compiledCondition) compiledA else compiledB
        }

      case Bool(b) => Contextual.Pure(b)

      case And(a, b) =>
        interpret2(a, b)(_ && _)

      case Or(a, b) =>
        interpret2(a, b)(_ || _)

      case Not(a) =>
        materializeExpr(a).map(!_)

      case GreaterThan(a, b, ord)        => interpret2(a, b)(ord.gt)
      case GreaterThanOrEqual(a, b, ord) => interpret2(a, b)(ord.gteqv)
      case LessThan(a, b, ord)           => interpret2(a, b)(ord.lt)
      case LessThanOrEqual(a, b, ord)    => interpret2(a, b)(ord.lteqv)

      case InRect(topLeft, bottomRight, p) =>
        interpret3(topLeft, bottomRight, p) { (compiledTopLeft, compiledBottomRight, compiledP) =>
          compiledP.inRectangle(compiledTopLeft, compiledBottomRight)
        }

      case Var(name) =>
        WithContext.instance[T] { ctx =>
          get[Any](ctx, name).asInstanceOf[T]
        }

      case Let(name, value, e) =>
        materializeExpr(App(Lambda(name, e), value))

      case lambda: Lambda[s, t] =>
        val interpretedBody = materializeExpr(lambda.expr)
        WithContext.instance[T] { ctx => (a: s) =>
          interpretedBody(addStrict(lambda.variable, a, ctx))
        }

      case App(f, a) => interpret2(f, a)(_(_))

      case Expr.Constant(t) =>
        interpret1(t)(Evolution.constant)

      case Fix(Lambda(name, lambdaBody)) =>
        val interpretedBody = materializeExpr(lambdaBody)
        WithContext.instance[T] { ctx =>
          {
            lazy val self: T = interpretedBody(addLazy(name, () => self, ctx))
            self
          }
        }

      case Fix(_) => ???

      case Empty() =>
        Contextual.Pure(Evolution.empty)

      // TODO we need a well-defined strategy for lazyness. In this case, we delay the materialization of cons, to allow
      // recursive definitions
      case Cons(head, tail) =>
        val interpretedHead = materializeExpr(head)
        val interpretedTail = materializeExpr(tail)
        WithContext.instance[T] { ctx =>
          Evolution.cons(interpretedHead(ctx), interpretedTail(ctx))
        }

      case Concat(ev1, ev2) =>
        interpret2(ev1, ev2)(Evolution.concat)

      case MapEmpty(ev1, ev2) =>
        interpret2(ev1, ev2)(Evolution.mapEmpty)

      case MapCons(eva, f) =>
        interpret2(eva, f)(Evolution.mapCons)

      case ZipWith(fa, fb, f) =>
        interpret3(fa, fb, f)(Evolution.zipWith)

      case Take(nExpr, faExpr) =>
        interpret2(nExpr, faExpr)(Evolution.take)

      case TakeWhile(fa, p) =>
        interpret2(fa, p)(Evolution.takeWhile)

      case WithFirst(as, f) =>
        interpret2(as, f)(Evolution.withFirst1)

      case FlatMap(faExpr, fExpr) => interpret2(faExpr, fExpr)(Evolution.flatMap)

      case Flatten(ffa) => interpret1(ffa)(Evolution.flatten)

      case Parallel(ffa) => interpret1(ffa)(Evolution.parallel)

      case Map(fa, f) => interpret2(fa, f)(Evolution.map)

      case MapWithDerivative(fa, f, sg, inv) =>
        interpret2(fa, f)(Evolution.mapWithDerivative(_, _, MaterializeAddition(sg), inv))

      case Range(from, to, step) => interpret3(from, to, step)(Evolution.range)

      case Uniform(from, to) =>
        interpret2(from, to)(Evolution.uniform)

      case UniformChoice(choices) =>
        interpret1(choices)(Evolution.uniformChoice)

      case UniformDiscrete(from, to, step) =>
        interpret3(from, to, step)(Evolution.uniformDiscrete)

      case UniformFrom(n, ft) =>
        interpret2(n, ft)(Evolution.uniformFrom)

      case Integrate(startExpr, speedExpr, semigroup) =>
        interpret2(startExpr, speedExpr)(
          (start, speed) => Evolution.integrate(start, speed, MaterializeAddition(semigroup))
        )

      case Solve1(speedExpr, startExpr, semigroup) =>
        interpret2(speedExpr, startExpr)(
          (speed, start) => Evolution.solve1(speed, start, MaterializeAddition(semigroup))
        )

      case Solve2(accExpr, startExpr, speedExpr, semigroup) =>
        interpret3(accExpr, startExpr, speedExpr)(
          (acc, start, speed) => Evolution.solve2(acc, start, speed, MaterializeAddition(semigroup))
        )

      case Derive(t, sg, inv) => interpret1(t)(Evolution.derive(_, MaterializeAddition(sg), inv))

      case Normal(μ, σ) =>
        interpret2(μ, σ)(Evolution.normal)

      case Noise() => Contextual.Pure(Evolution.noiseEvolution)

      case OctaveNoise() => Contextual.Pure(Evolution.octaveNoiseEvolution)
    }
  }

  private def interpret1[A, B](a: Expr[A])(f: A => B): Contextual[B] =
    materializeExpr(a).map(f)

  private def interpret2[A, B, C](a: Expr[A], b: Expr[B])(f: (A, B) => C): Contextual[C] =
    Contextual.map2(materializeExpr(a), materializeExpr(b))(f)

  private def interpret3[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C])(f: (A, B, C) => D): Contextual[D] =
    Contextual.map3(materializeExpr(a), materializeExpr(b), materializeExpr(c))(f)

  private def interpret3Lazy[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C])(
    f: (=> A, => B, => C) => D
  ): Contextual[D] =
    Contextual.map3Lazy(materializeExpr(a), materializeExpr(b), materializeExpr(c))(f)
}
