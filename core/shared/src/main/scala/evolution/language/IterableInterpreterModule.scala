package evolution.language
import evolution.data.EvaluationContext._
import evolution.data.{ Ctx, ExpressionModule }
import evolution.geometry.Point
import evolution.materialization.Iterable

// TODO this is an implementation
trait IterableInterpreterModule { self: ExpressionModule[Iterable] =>
  type Out[T] = IterableInterpreterModule.Contextual[T]
  import Expr._
  import IterableInterpreterModule._

  object Interpreter {

    def interpret[T](expr: Expr[T]): Out[T] = {
      expr match {
        case Dbl(d)            => Contextual.Pure(d)
        case Floor(d)          => interpret(d).map(_.toInt)
        case ToDbl(n)          => interpret(n).map(_.toDouble)
        case Integer(n)        => Contextual.Pure(n)
        case Pnt(x, y)         => interpret2(x, y)(Point.apply)
        case LiftedPnt(x, y)   => interpret2(x, y)(Iterable.zipWithUncurried(Point.apply))
        case Polar(x, y)       => interpret2(x, y)(Point.polar)
        case LiftedPolar(x, y) => interpret2(x, y)(Iterable.zipWithUncurried(Point.polar))
        case X(p)              => interpret1(p)(_.x)
        case Y(p)              => interpret1(p)(_.y)
        case Norm(p)           => interpret1(p)(_.norm)
        case Versor(p)         => interpret1(p)(_.versor)
        case e @ Add(_, _, _)  => interpret2(e.a, e.b)(e.add.combine)
        case Div(a, b)         => interpret2(a, b)(_ / _)
        case Exp(a, b)         => interpret2(a, b)(Math.pow)
        case Abs(a)            => interpret(a).map(Math.abs)
        case Sign(a)           => interpret(a).map(Math.signum)
        case Mod(a, b) =>
          interpret2(a, b) { (ca, cb) =>
            if (ca >= 0) ca % cb else (ca % cb) + cb
          }
        case e @ Inverse(_, _)     => interpret1(e.t)(e.inv.inverse)
        case e @ Minus(_, _, _, _)  => interpret2(e.a, e.b)((a, b) => e.sg.combine(a, e.inv.inverse(b)))
        case e @ Multiply(_, _, _) => interpret2(e.a, e.b)(e.mult.combine)
        case Sin(d)                => interpret1(d)(Math.sin)
        case Cos(d)                => interpret1(d)(Math.cos)
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
          interpret3(condition, a, b) { (compiledCondition, compiledA, compiledB) =>
            if (compiledCondition) compiledA else compiledB
          }

        case Bool(b) => Contextual.Pure(b)

        case And(a, b) =>
          interpret2(a, b)(_ && _)

        case Or(a, b) =>
          interpret2(a, b)(_ || _)

        case Not(a) =>
          interpret(a).map(!_)

        case GreaterThan(a, b, ord)        => interpret2(a, b)(ord.gt)
        case GreaterThanOrEqual(a, b, ord) => interpret2(a, b)(ord.gteqv)
        case LessThan(a, b, ord)           => interpret2(a, b)(ord.lt)
        case LessThanOrEqual(a, b, ord)    => interpret2(a, b)(ord.lteqv)

        case InRect(topLeft, bottomRight, p) =>
          interpret3(topLeft, bottomRight, p) { (compiledTopLeft, compiledBottomRight, compiledP) =>
            compiledP.inRectangle(compiledTopLeft, compiledBottomRight)
          }

        case Var(name) =>
          new Contextual.WithContext[T] {
            override def apply(ctx: Ctx): T = get[Any](ctx, name).asInstanceOf[T]
          }

        case Let(name, value, e) =>
          interpret(App(Lambda(name, e), value))

        case lambda: Lambda[s, t] =>
          val interpretedBody = interpret(lambda.expr)
          new Contextual.WithContext[T] {
            override def apply(ctx: Ctx): T = (a: s) => interpretedBody(addStrict(lambda.variable, a, ctx))
          }

        case App(f, a) => interpret2(f, a)(_(_))

        // Detect constant evolutions
        case Expr.Constant(t) =>
          interpret1(t)(Iterable.constant)

        case Fix(Lambda(name, lambdaBody)) =>
          val interpretedBody = interpret(lambdaBody)
          new Contextual.WithContext[T] {
            override def apply(ctx: Ctx): T = {
              lazy val self: T = interpretedBody(addLazy(name, () => self, ctx))
              self
            }
          }

        case Fix(_) => ???

        case Empty() =>
          Contextual.Pure(Iterable.empty)

        // TODO we need a well-defined strategy for lazyness. In this case, we delay the materialization of cons, to allow
        // recursive definitions
        case Cons(head, tail) =>
          val interpretedHead = interpret(head)
          val interpretedTail = interpret(tail)
          new Contextual.WithContext[T] {
            override def apply(ctx: Ctx): T = {
              Iterable.cons(interpretedHead(ctx), interpretedTail(ctx))
            }
          }

        case Concat(ev1, ev2) =>
          interpret2(ev1, ev2)(Iterable.concat)

        case MapEmpty(ev1, ev2) =>
          interpret2(ev1, ev2)(Iterable.mapEmpty)

        case MapCons(eva, f) =>
          interpret2(eva, f)(Iterable.mapCons)

        case ZipWith(fa, fb, f) =>
          interpret3(fa, fb, f)(Iterable.zipWith)

        case Take(nExpr, faExpr) =>
          interpret2(nExpr, faExpr)(Iterable.take)

        case TakeWhile(fa, p) =>
          interpret2(fa, p)(Iterable.takeWhile)

        case WithFirst(as, f) =>
          interpret2(as, f)(Iterable.withFirst1)

        case WithFirst2(as, f) =>
          interpret2(as, f)(Iterable.withFirst2)

        case WithFirst3(as, f) =>
          interpret2(as, f)(Iterable.withFirst3)

        case FlatMap(faExpr, fExpr) => interpret2(faExpr, fExpr)(Iterable.flatMap)

        case Flatten(ffa) => interpret1(ffa)(Iterable.flatten)

        case Parallel(ffa) => interpret1(ffa)(Iterable.parallel)

        case Map(fa, f) => interpret2(fa, f)(Iterable.map)

        case MapWithDerivative(fa, f, sg, inv) => interpret2(fa, f)(Iterable.mapWithDerivative(_, _, sg, inv))

        case Range(from, to, step) => interpret3(from, to, step)(Iterable.range)

        case Uniform(from, to) =>
          interpret2(from, to)(Iterable.uniform)

        case UniformDiscrete(from, to, step) =>
          interpret3(from, to, step)(Iterable.uniformDiscrete)

        case UniformFrom(n, ft) =>
          interpret2(n, ft)(Iterable.uniformFrom)

        case Integrate(startExpr, speedExpr, semigroup) =>
          interpret2(startExpr, speedExpr)((start, speed) => Iterable.integrate(start, speed, semigroup))

        case Solve1(speedExpr, startExpr, semigroup) =>
          interpret2(speedExpr, startExpr)((speed, start) => Iterable.solve1(speed, start, semigroup))

        case Solve2(accExpr, startExpr, speedExpr, semigroup) =>
          interpret3(accExpr, startExpr, speedExpr)(
            (acc, start, speed) => Iterable.solve2(acc, start, speed, semigroup)
          )

        case Derive(t, sg, inv) => interpret1(t)(Iterable.derive(_, sg, inv))

        case Normal(μ, σ) =>
          interpret2(μ, σ)(Iterable.normal)

        case Noise() => Contextual.Pure(Iterable.noiseIterable)

        case OctaveNoise() => Contextual.Pure(Iterable.octaveNoiseIterable)
      }
    }

    private def interpret1[A, B](a: Expr[A])(f: A => B): Out[B] =
      interpret(a).map(f)

    private def interpret2[A, B, C](a: Expr[A], b: Expr[B])(f: (A, B) => C): Out[C] =
      Contextual.map2(interpret(a), interpret(b))(f)

    private def interpret3[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C])(f: (A, B, C) => D): Out[D] =
      Contextual.map3(interpret(a), interpret(b), interpret(c))(f)
  }
}

object IterableInterpreterModule {
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
  }
}
