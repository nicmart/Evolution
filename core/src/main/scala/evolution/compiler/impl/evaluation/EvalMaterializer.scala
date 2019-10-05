package evolution.compiler.impl.evaluation

import evolution.materialization.Evolution
import evolution.data.EvaluationContext._
import evolution.compiler.expression.Expr
import evolution.geometry.Point
import evolution.compiler.phases.materializing.model.Contextual
import evolution.compiler.phases.materializing.model.Contextual.WithContext
import evolution.data.emptyCtx
import evolution.compiler.phases.materializing.Materializer
import evolution.compiler.expression.Expr.SlidingMap
import evolution.compiler.expression.Expr.Iterate

// TODO this is an implementation
object EvalMaterializer extends Materializer {

  def materialize[T](expr: Expr[Evolution[T]]): Long => Iterator[T] =
    seed => Evolution.runWithSeed(seed, materializeExpr(expr).apply(emptyCtx))

  def materializeExpr[T](expr: Expr[T]): Contextual[T] = {
    expr match {
      case Expr.Dbl(d)      => Contextual.Pure(d)
      case Expr.Floor(d)    => materializeExpr(d).map(_.toInt)
      case Expr.ToDouble(n) => materializeExpr(n).map(_.toDouble)
      case Expr.Integer(n)  => Contextual.Pure(n)
      case Expr.Pnt(x, y)   => interpret2(x, y)(Point.apply)
      case Expr.LiftedPnt(x, y) =>
        interpret2[Evolution[Double], Evolution[Double], Evolution[Point]](x, y)(
          Evolution.zipWithUncurried(Point.apply)
        )
      case Expr.Polar(x, y)       => interpret2(x, y)(Point.polar)
      case Expr.LiftedPolar(x, y) => interpret2(x, y)(Evolution.zipWithUncurried(Point.polar))
      case Expr.X(p)              => interpret1(p)(_.x)
      case Expr.Y(p)              => interpret1(p)(_.y)
      case Expr.Norm(p)           => interpret1(p)(_.norm)
      case Expr.Versor(p)         => interpret1(p)(_.versor)
      case Expr.Add(a, b, add)    => interpret2(a, b)(MaterializeAddition(add))
      case Expr.Div(a, b)         => interpret2(a, b)(_ / _)
      case Expr.Exp(a, b)         => interpret2(a, b)(Math.pow)
      case Expr.Abs(a)            => materializeExpr(a).map(Math.abs)
      case Expr.Sign(a)           => materializeExpr(a).map(Math.signum)
      case Expr.Mod(a, b) =>
        interpret2(a, b) { (ca, cb) =>
          if (ca >= 0) ca % cb else (ca % cb) + cb
        }
      case e @ Expr.Inverse(_, _) => interpret1(e.t)(MaterializeInverse(e.inv))
      case e @ Expr.Minus(_, _, _, _) =>
        interpret2(e.a, e.b)((a, b) => MaterializeAddition(e.sg)(a, MaterializeInverse(e.inv)(b)))
      case e @ Expr.Multiply(_, _, _) => interpret2(e.a, e.b)(MaterializeMultiplication(e.mult))
      case Expr.Sin(d)                => interpret1(d)(Math.sin)
      case Expr.Cos(d)                => interpret1(d)(Math.cos)
      case Expr.Lst(ts)               => Contextual.lst(ts.map(materializeExpr))
      case Expr.SmoothStep(f, t, p) =>
        interpret3(f, t, p) { (from, to, position) =>
          val t = (position - from) / (to - from)
          if (t <= 0) 0.0
          else if (t >= 1) 1.0
          else t * t * (3.0 - 2.0 * t)
        }
      case Expr.Equals(a, b, eq) => interpret2(a, b)(MaterializeEquality(eq).eqv)
      case Expr.Neq(a, b, eq)    => interpret2(a, b)(MaterializeEquality(eq).neqv)
      case Expr.IfThen(condition, a, b) =>
        interpret3Lazy(condition, a, b) { (compiledCondition, compiledA, compiledB) =>
          if (compiledCondition) compiledA else compiledB
        }

      case Expr.Bool(b) => Contextual.Pure(b)

      case Expr.And(a, b) =>
        interpret2(a, b)(_ && _)

      case Expr.Or(a, b) =>
        interpret2(a, b)(_ || _)

      case Expr.Not(a) =>
        materializeExpr(a).map(!_)

      case Expr.GreaterThan(a, b, cmp)        => interpret2(a, b)(MaterializeComparison(cmp).gt)
      case Expr.GreaterThanOrEqual(a, b, cmp) => interpret2(a, b)(MaterializeComparison(cmp).gteqv)
      case Expr.LessThan(a, b, cmp)           => interpret2(a, b)(MaterializeComparison(cmp).lt)
      case Expr.LessThanOrEqual(a, b, cmp)    => interpret2(a, b)(MaterializeComparison(cmp).lteqv)

      case Expr.InRect(topLeft, bottomRight, p) =>
        interpret3(topLeft, bottomRight, p) { (compiledTopLeft, compiledBottomRight, compiledP) =>
          compiledP.inRectangle(compiledTopLeft, compiledBottomRight)
        }

      case Expr.Var(name) =>
        WithContext.instance[T] { ctx =>
          get[Any](ctx, name).asInstanceOf[T]
        }

      case Expr.Let(name, value, e) =>
        materializeExpr(Expr.App(Expr.Lambda(name, e), value))

      case lambda: Expr.Lambda[s, t] =>
        val interpretedBody = materializeExpr(lambda.expr)
        WithContext.instance[T] { ctx => (a: s) =>
          interpretedBody(addStrict(lambda.variable, a, ctx))
        }

      case Expr.App(f, a) => interpret2(f, a)(_(_))

      case Expr.Constant(t) =>
        interpret1(t)(Evolution.constant)

      case Expr.Fix(Expr.Lambda(name, lambdaBody)) =>
        val interpretedBody = materializeExpr(lambdaBody)
        WithContext.instance[T] { ctx =>
          {
            lazy val self: T = interpretedBody(addLazy(name, () => self, ctx))
            self
          }
        }

      case Expr.Fix(_) => ???

      case Expr.Empty() =>
        Contextual.Pure(Evolution.empty)

      // TODO we need a well-defined strategy for lazyness. In this case, we delay the materialization of cons, to allow
      // recursive definitions
      case Expr.Cons(head, tail) =>
        val interpretedHead = materializeExpr(head)
        val interpretedTail = materializeExpr(tail)
        WithContext.instance[T] { ctx =>
          Evolution.cons(interpretedHead(ctx), interpretedTail(ctx))
        }

      case Expr.Concat(ev1, ev2) =>
        interpret2(ev1, ev2)(Evolution.concat)

      case Expr.ZipWith(fa, fb, f) =>
        interpret3(fa, fb, f)(Evolution.zipWith)

      case Expr.Take(nExpr, faExpr) =>
        interpret2(nExpr, faExpr)(Evolution.take)

      case Expr.TakeWhile(fa, p) =>
        interpret2(fa, p)(Evolution.takeWhile)

      case Expr.WithFirst(as, f) =>
        interpret2(as, f)(Evolution.withFirst1)

      case Expr.FlatMap(faExpr, fExpr) => interpret2(faExpr, fExpr)(Evolution.flatMap)

      case Expr.Flatten(ffa) => interpret1(ffa)(Evolution.flatten)

      case Expr.Parallel(ffa) => interpret1(ffa)(Evolution.parallel)

      case Expr.Map(fa, f) => interpret2(fa, f)(Evolution.map)

      case Expr.MapWithDerivative(fa, f, sg, inv) =>
        interpret2(fa, f)(Evolution.mapWithDerivative(_, _, MaterializeAddition(sg), MaterializeInverse(inv)))

      case SlidingMap(fa, f) => interpret2(fa, f)(Evolution.slidingMap)

      case Iterate(f, start) => interpret2(f, start)(Evolution.iterate)

      case Expr.Range(from, to, step) => interpret3(from, to, step)(Evolution.range)

      case Expr.Uniform(from, to) =>
        interpret2(from, to)(Evolution.uniform)

      case Expr.UniformChoice(choices) =>
        interpret1(choices)(Evolution.uniformChoice)

      case Expr.UniformDiscrete(from, to, step) =>
        interpret3(from, to, step)(Evolution.uniformDiscrete)

      case Expr.UniformFrom(n, ft) =>
        interpret2(n, ft)(Evolution.uniformFrom)

      case Expr.Integrate(startExpr, speedExpr, semigroup) =>
        interpret2(startExpr, speedExpr)(
          (start, speed) => Evolution.integrate(start, speed, MaterializeAddition(semigroup))
        )

      case Expr.Solve1(speedExpr, startExpr, semigroup) =>
        interpret2(speedExpr, startExpr)(
          (speed, start) => Evolution.solve1(speed, start, MaterializeAddition(semigroup))
        )

      case Expr.Solve2(accExpr, startExpr, speedExpr, semigroup) =>
        interpret3(accExpr, startExpr, speedExpr)(
          (acc, start, speed) => Evolution.solve2(acc, start, speed, MaterializeAddition(semigroup))
        )

      case Expr.Derive(t, sg, inv) =>
        interpret1(t)(Evolution.derive(_, MaterializeAddition(sg), MaterializeInverse(inv)))

      case Expr.Normal(μ, σ) =>
        interpret2(μ, σ)(Evolution.normal)

      case Expr.Noise() => Contextual.Pure(Evolution.noiseEvolution)

      case Expr.OctaveNoise() => Contextual.Pure(Evolution.octaveNoiseEvolution)
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
