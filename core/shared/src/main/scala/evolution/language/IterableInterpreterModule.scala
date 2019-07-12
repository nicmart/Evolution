package evolution.language
import evolution.data.EvaluationContext._
import evolution.data.{ Ctx, ExpressionModule }
import evolution.geometry.Point
import evolution.rng.PerlinNoise
import evolution.materialization.Iterable
import scala.collection.AbstractIterator

// TODO this is an implementation
trait IterableInterpreterModule { self: ExpressionModule[Iterable] =>
  type Out[T] = IterableInterpreterModule.Out[T]
  import Expr._
  import IterableInterpreterModule._

  object Interpreter {

    def interpret[T](expr: Expr[T]): Out[T] = {
      _runs = _runs + 1
      expr match {
        case Dbl(d)          => Out.pure(d)
        case Floor(d)        => interpret(d).map(_.toInt)
        case ToDbl(n)        => interpret(n).map(_.toDouble)
        case Integer(n)      => Out.pure(n)
        case Pnt(x, y)       => interpret2(x, y)(Point.apply)
        case X(p)            => interpret1(p)(_.x)
        case Y(p)            => interpret1(p)(_.y)
        case add @ Add(a, b) => interpret2(a, b)(add.semigroup.combine)
        case Div(a, b)       => interpret2(a, b)(_ / _)
        case Exp(a, b)       => interpret2(a, b)(Math.pow)
        case Abs(a)          => interpret(a).map(Math.abs)
        case Sign(a)         => interpret(a).map(Math.signum)
        case Mod(a, b) =>
          interpret2(a, b) { (ca, cb) =>
            if (ca >= 0) ca % cb else (ca % cb) + cb
          }
        case inv @ Inverse(t)      => interpret1(t)(inv.group.inverse)
        case mult @ Multiply(k, t) => interpret2(k, t)(mult.vectorSpace.mult)
        case Sin(d)                => interpret1(d)(Math.sin)
        case Cos(d)                => interpret1(d)(Math.cos)
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

        case Lambda(name, body) =>
          val interpretedBody = interpret(body)
          new Contextual[T] {
            override def apply(ctx: Ctx): T = a => interpretedBody(addStrict(name, a, ctx))
          }

        case App(f, a) => interpret2(f, a)(_(_))

        // Detect constant evolutions
        case Fix(Lambda(_, Cons(t, Var(_)))) =>
          ConstantEvolution(interpret(t))

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

        case MapEmpty(ev1, ev2) =>
          interpret2(ev1, ev2)(Iterable.mapEmpty)

        case MapCons(eva, f) =>
          interpret2(eva, f)(Iterable.mapCons)
        // interpret2(eva, f) { (compiledEva, compiledF) =>
        //   Iterable { rng =>
        //     val (rng2, next) = compiledEva.run(rng)
        //     next match {
        //       case None            => (rng2, None)
        //       case Some((a, eva2)) => compiledF(a)(eva2).run(rng2)
        //     }
        //   }
        // }

        case ZipWith(fa, fb, f) =>
          interpret3(fa, fb, f)(Iterable.zipWith)

        case Take(nExpr, faExpr) =>
          interpret2(nExpr, faExpr)(Iterable.take)

        case TakeWhile(fa, p) =>
          interpret2(fa, p)(Iterable.takeWhile)

        case FlatMap(faExpr, fExpr) => interpret2(faExpr, fExpr)(Iterable.flatMap)

        case Flatten(ffa) => interpret1(ffa)(Iterable.flatten)

        case Map(fa, f) => interpret2(fa, f)(Iterable.map)

        case Uniform(from, to) =>
          interpret2(from, to)(Iterable.uniform)

        case UniformDiscrete(from, to, step) =>
          interpret3(from, to, step)(Iterable.uniformDiscrete)

        case UniformFrom(n, ft) =>
          interpret2(n, ft)(Iterable.uniformFrom)

        case Integrate(startExpr, speedExpr, vectorSpace) =>
          interpret2(startExpr, speedExpr)((start, speed) => Iterable.integrate(start, speed, vectorSpace))

        // See https://en.wikipedia.org/wiki/Box%E2%80%93Muller_transform#Implementation
        case Normal(μ, σ) =>
          interpret2(μ, σ)(Iterable.normal)

        // TODO
        case Noise() => ???
        //Constant(PerlinNoise.noiseIterable)

        // TODO
        case OctaveNoise() => ???
        //Constant(PerlinNoise.octaveNoiseIterable)
      }
    }

    private def interpret1[A, B](a: Expr[A])(f: A => B): Out[B] =
      interpret(a).map(f)

    private def interpret2[A, B, C](a: Expr[A], b: Expr[B])(f: (A, B) => C): Out[C] =
      Out.map2(interpret(a), interpret(b))(f)

    private def interpret3[A, B, C, D](a: Expr[A], b: Expr[B], c: Expr[C])(f: (A, B, C) => D): Out[D] =
      Out.map3(interpret(a), interpret(b), interpret(c))(f)
  }

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

  sealed trait Out[T] { self =>
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
