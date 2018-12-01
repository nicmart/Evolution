package evolution.primitive.algebra.binding.interpreter
import cats.Applicative
import evolution.primitive.algebra.binding.Binding
import evolution.primitive.algebra.binding.interpreter.BindingEvaluator.{ app, fix }
import evolution.primitive.algebra.binding.interpreter.EvaluationResult._

import scala.util.Random

object BindingEvaluator extends Binding[EvaluationResult, String] {
  override def var0[A]: EvaluationResult[A] = Var(0)

  override def shift[A](expr: EvaluationResult[A]): EvaluationResult[A] = expr match {
    case Var(n) => Var(n + 1)
    case _      => Value(ctx => expr.get(ctx.tail))
  }
  override def let[A, B](name: String, value: EvaluationResult[A], expr: EvaluationResult[B]): EvaluationResult[B] =
    app(lambda(name, expr), value)

  override def fix[A](expr: EvaluationResult[A => A]): EvaluationResult[A] =
    Fix(expr)

  override def lambda[A, B](name: String, expr: EvaluationResult[B]): EvaluationResult[A => B] =
    Lam(name, expr)

  override def app[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]): EvaluationResult[B] =
    f match {
      case lambda @ Lam(_, _)                    => AppOfLambda(lambda, a)
      case AppOfLambda(Lam(_, Lam(_, inner)), b) => App2OfLambda[A, Any, B](inner, b, a)
      case _                                     => App(f, a)
    }
}

sealed trait EvaluationResult[T] {
  @inline def get(ctx: Ctx): T =
    //debug(s"Evaluating $this", evaluate(ctx))
    evaluate(ctx)
  @inline protected def evaluate(ctx: Ctx): T
}

object EvaluationResult {
  type Ctx = List[() => Any]
  def push[T](elem: () => T, ctx: Ctx): Ctx = elem :: ctx
  def pushStrict[T](elem: T, ctx: Ctx): Ctx = (() => elem) :: ctx

  case class Constant[A](a: A) extends EvaluationResult[A] {
    override def evaluate(ctx: Ctx): A = a
  }

  case class App[A, B](f: EvaluationResult[A => B], a: EvaluationResult[A]) extends EvaluationResult[B] {
    override def evaluate(ctx: Ctx): B = f.get(ctx)(a.get(ctx))
  }

  case class AppOfLambda[A, B](f: Lam[A, B], a: EvaluationResult[A]) extends EvaluationResult[B] {
    private val expr = f.term
    override protected def evaluate(ctx: Ctx): B = expr.get(pushStrict(a.get(ctx), ctx))
  }

  case class App2OfLambda[A, B, C](inner: EvaluationResult[C], b: EvaluationResult[B], a: EvaluationResult[A])
      extends EvaluationResult[C] {
    override protected def evaluate(ctx: Ctx): C = inner.get(pushStrict(b.get(ctx), pushStrict(a.get(ctx), ctx)))
  }

  case class Fix[A](expr: EvaluationResult[A => A]) extends EvaluationResult[A] {
    override def evaluate(ctx: Ctx): A = expr match {
      case Lam(_, term) => fixTerm(term.get)(ctx)
      case _            => app(expr, fix(expr)).get(ctx)
    }

    private def fixTerm(expr: Ctx => A): Ctx => A =
      ctx => {
        lazy val a: A = expr(push(() => a, ctx))
        debug("evaluating fix\nevaluating lambda of fix", a)
      }
  }

  case class Lam[A, B](varName: String, term: EvaluationResult[B]) extends EvaluationResult[A => B] {
    println("Creating Lambda")
    @inline override def evaluate(ctx: Ctx): A => B =
      a => term.get(pushStrict(a, ctx))
    override def toString: String = s"lambda($varName -> $term)"
  }
  case class Value[A](getA: Ctx => A, override val toString: String = "?") extends EvaluationResult[A] {
    @inline override def evaluate(ctx: Ctx): A = getA(ctx)
  }

  case class Var[A](n: Int) extends EvaluationResult[A] {
    @inline override def evaluate(ctx: Ctx): A = ctx(n)().asInstanceOf[A]
    override def toString: String = s"Var($n)"
  }

  implicit val applicative: Applicative[EvaluationResult] = new Applicative[EvaluationResult] {
    override def pure[A](x: A): EvaluationResult[A] = Value(_ => x)
    override def ap[A, B](ff: EvaluationResult[A => B])(fa: EvaluationResult[A]): EvaluationResult[B] =
      BindingEvaluator.app(ff, fa)
  }

  def debug[T](message: String, t: => T): T = {
    val id = Random.nextInt().toHexString.take(6)
    println(s"START ($id): $message")
    val result = t
    println(s"END   ($id): $message")
    result
  }
}
