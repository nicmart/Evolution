package evolution.language
import evolution.data.EvaluationContext._
import evolution.data.{ Ctx, ExpressionModule }
import evolution.geometry.Point
import evolution.materialization.Iterable

// TODO this is an implementation
trait IterableInterpreterModule { self: ExpressionModule[Iterable] =>
  type Out[T] = IterableInterpreterModule.Out[T]
  import Expr._
  import IterableInterpreterModule._

  object Interpreter {

    def interpret[T](expr: Expr[T]): Out[T] = {
      _runs = _runs + 1
      expr match {
        case Dbl(d)            => Out.pure(d)
        case Floor(d)          => interpret(d).map(_.toInt)
        case ToDbl(n)          => interpret(n).map(_.toDouble)
        case Integer(n)        => Out.pure(n)
        case Pnt(x, y)         => interpret2(x, y)(Point.apply)
        case LiftedPnt(x, y)   => interpret2(x, y)(Iterable.zipWithUncurried(Point.apply))
        case Polar(x, y)       => interpret2(x, y)(Point.polar)
        case LiftedPolar(x, y) => interpret2(x, y)(Iterable.zipWithUncurried(Point.polar))
        case X(p)              => interpret1(p)(_.x)
        case Y(p)              => interpret1(p)(_.y)
        case Norm(p)           => interpret1(p)(_.norm)
        case Versor(p)         => interpret1(p)(_.versor)
        case add: Add[_]       => interpret2(add.a, add.b)(add.semigroup.combine)
        case add: LiftedAdd[_] => interpret2(add.a, add.b)(Iterable.zipWithUncurried(add.semigroup.combine))
        case Div(a, b)         => interpret2(a, b)(_ / _)
        case Exp(a, b)         => interpret2(a, b)(Math.pow)
        case Abs(a)            => interpret(a).map(Math.abs)
        case Sign(a)           => interpret(a).map(Math.signum)
        case Mod(a, b) =>
          interpret2(a, b) { (ca, cb) =>
            if (ca >= 0) ca % cb else (ca % cb) + cb
          }
        case inv: Inverse[_]         => interpret1(inv.t)(inv.group.inverse)
        case mult: Multiply[_]       => interpret2(mult.k, mult.t)(mult.vectorSpace.multiply)
        case mult: LiftedMultiply[_] => interpret2(mult.k, mult.t)(Iterable.zipWithUncurried(mult.vectorSpace.multiply))
        case Sin(d)                  => interpret1(d)(Math.sin)
        case Cos(d)                  => interpret1(d)(Math.cos)
        case SmoothStep(f, t, p) =>
          interpret3(f, t, p) { (from, to, position) =>
            val t = (position - from) / (to - from)
            if (t <= 0) 0.0
            else if (t >= 1) 1.0
            else t * t * (3.0 - 2.0 * t)
          }
        case eq @ Equals(a, b) => interpret2(a, b)(eq.eq.eqv)
        case neq @ Neq(a, b)   => interpret2(a, b)(neq.eq.neqv)
        case IfThen(condition, a, b) =>
          interpret3(condition, a, b) { (compiledCondition, compiledA, compiledB) =>
            if (compiledCondition) compiledA else compiledB
          }

        case Bool(b) => Out.pure(b)

        case And(a, b) =>
          interpret2(a, b)(_ && _)

        case Or(a, b) =>
          interpret2(a, b)(_ || _)

        case Not(a) =>
          interpret(a).map(!_)

        case expr @ GreaterThan(a, b)        => interpret2(a, b)(expr.ord.gt)
        case expr @ GreaterThanOrEqual(a, b) => interpret2(a, b)(expr.ord.gteqv)
        case expr @ LessThan(a, b)           => interpret2(a, b)(expr.ord.lt)
        case expr @ LessThanOrEqual(a, b)    => interpret2(a, b)(expr.ord.lteqv)

        case InRect(topLeft, bottomRight, p) =>
          interpret3(topLeft, bottomRight, p) { (compiledTopLeft, compiledBottomRight, compiledP) =>
            compiledP.inRectangle(compiledTopLeft, compiledBottomRight)
          }

        case Var(name) =>
          new Contextual[T] {
            override def apply(ctx: Ctx): T = get[Any](ctx, name).asInstanceOf[T]
          }

        case Let(name, value, e) =>
          interpret(App(Lambda(name, e), value))

        case lambda: Lambda[s, t] =>
          val interpretedBody = interpret(lambda.expr)
          new Contextual[T] {
            override def apply(ctx: Ctx): T = (a: s) => interpretedBody(addStrict(lambda.variable, a, ctx))
          }

        case App(f, a) => interpret2(f, a)(_(_))

        // Detect constant evolutions
        case Expr.Constant(t) =>
          interpret1(t)(Iterable.constant)

        case Fix(Lambda(name, lambdaBody)) =>
          val interpretedBody = interpret(lambdaBody)
          new Contextual[T] {
            override def apply(ctx: Ctx): T = {
              lazy val self: T = interpretedBody(addLazy(name, () => self, ctx))
              self
            }
          }

        case Fix(_) => ???

        case Empty() =>
          Out.pure(Iterable.empty)

        // TODO we need a well-defined strategy for lazyness. In this case, we delay the materialization of cons, to allow
        // recursive definitions
        case Cons(head, tail) =>
          val interpretedHead = interpret(head)
          val interpretedTail = interpret(tail)
          new Contextual[T] {
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

        //case ZipWith(a, b, Lambda(f, Lambda(x, App(Var(fVar), Var(xVar))))) if f == fVar && x == xVar => ???

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

        case Map(fa, f) => interpret2(fa, f)(Iterable.map)

        case MapWithDerivative(fa, f, group) => interpret2(fa, f)(Iterable.mapWithDerivative(_, _, group))

        case Range(from, to, step) => interpret3(from, to, step)(Iterable.range)

        case Uniform(from, to) =>
          interpret2(from, to)(Iterable.uniform)

        case UniformDiscrete(from, to, step) =>
          interpret3(from, to, step)(Iterable.uniformDiscrete)

        case UniformFrom(n, ft) =>
          interpret2(n, ft)(Iterable.uniformFrom)

        case Integrate(startExpr, speedExpr, vectorSpace) =>
          interpret2(startExpr, speedExpr)((start, speed) => Iterable.integrate(start, speed, vectorSpace))

        case Solve1(speedExpr, startExpr, vectorSpace) =>
          interpret2(speedExpr, startExpr)((speed, start) => Iterable.solve1(speed, start, vectorSpace))

        case Solve2(accExpr, startExpr, speedExpr, vectorSpace) =>
          interpret3(accExpr, startExpr, speedExpr)(
            (acc, start, speed) => Iterable.solve2(acc, start, speed, vectorSpace)
          )

        case Derive(t, vectorSpace) => interpret1(t)(Iterable.derive(_, vectorSpace))

        case Normal(μ, σ) =>
          interpret2(μ, σ)(Iterable.normal)

        case Noise() => IterableInterpreterModule.Constant(Iterable.noiseIterable)

        case OctaveNoise() => IterableInterpreterModule.Constant(Iterable.octaveNoiseIterable)
      }
    }

    private def interpret1[A, B](a: Expr[A])(f: A => B): Out[B] =
      interpret(a).map(f)

    private def interpret2[A, B, C](a: Expr[A], b: Expr[B])(f: (A, B) => C): Out[C] =
      Out.map2(interpret(a), interpret(b))(f)

    private def interpret3[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C])(f: (A, B, C) => D): Out[D] =
      Out.map3(interpret(a), interpret(b), interpret(c))(f)
  }

  // private def extractVariadicZipWith(zipWith: ZipWith[Any, Any, Any]): Option[(Seq[Iterable[Any]], Seq[Any] => Any)] = {
  //   def isApplyLambda[T](expr: Expr[T]): Boolean = expr match {
  //     case Lambda(f, Lambda(x, App(Var(fVar), Var(xVar)))) if f == fVar && x == xVar => true
  //     case _                                                                         => false
  //   }
  //   ???
  // }

  private var _runs = 0
  def resetCounts(): Unit = {
    _runs = 0
    IterableInterpreterModule.outAllocations = 0
  }
  def interpreterRuns: Int = _runs
  def outAllocations: Int = IterableInterpreterModule.outAllocations
}

object IterableInterpreterModule {
  private var outAllocations: Int = 0

  sealed trait Out[+T] { self =>
    outAllocations += 1
    def apply(ctx: Ctx): T
    final def map[S](f: T => S): Out[S] = Out.map(this, f)
  }

  case class Constant[T](t: T) extends Out[T] {
    override def apply(ctx: Ctx): T = t
  }

  case class ConstantEvolution[T](t: Out[T]) extends Out[Iterable[T]] {
    override def apply(ctx: Ctx): Iterable[T] = {
      val tc = t(ctx)
      Iterable.constant(tc)
    }
  }

  sealed abstract class Contextual[T] extends Out[T]

  object Contextual {
    def instance[T](f: Ctx => T): Out[T] = new Contextual[T] {
      override def apply(ctx: Ctx): T = f(ctx)
    }
  }

  object Out {
    def pure[T](t: T): Out[T] = Constant(t)
    def map[A, B](a: Out[A], f: A => B): Out[B] = a match {
      case Constant(t) => Constant(f(t))
      case contextual  => new Contextual[B] { override def apply(ctx: Ctx): B = f(contextual(ctx)) }
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
